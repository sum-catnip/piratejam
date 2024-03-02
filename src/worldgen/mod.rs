use std::collections::HashMap;

use bevy::prelude::*;
use bevy_fast_tilemap::{FastTileMapPlugin, Map, MapBundleManaged, MapIndexer, AXONOMETRIC};
use bevy_inspector_egui::{prelude::*, quick::ResourceInspectorPlugin};

use noise::{NoiseFn, Simplex};

pub struct WorldGenPlugin;
impl Plugin for WorldGenPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, setup)
            .add_plugins(FastTileMapPlugin)
            .init_resource::<Chunks>()
            .init_resource::<WorldgenConfig>()
            .register_type::<WorldgenConfig>()
            .init_resource::<MapgridDebug>()
            .register_type::<MapgridDebug>()
            .add_plugins(ResourceInspectorPlugin::<WorldgenConfig>::default())
            .add_plugins(ResourceInspectorPlugin::<MapgridDebug>::default())
            .add_systems(Update, (spawn_chunks, despawn_chunks))
            .add_systems(Update, debug_view);
    }
}

#[derive(Resource)]
struct Tileset {
    tex: Handle<Image>,
    water: u32,
    sand: u32,
}

#[derive(Resource)]
struct TerrainNoise {
    simplex1: Simplex,
}

#[derive(Component)]
struct Chunk;

#[derive(Reflect, Resource, Default, InspectorOptions)]
#[reflect(Resource, InspectorOptions)]
struct WorldgenConfig {
    render_dist: u32,
    wavelength: f64,
    land_threshhold: f64,
}

#[derive(Reflect, Resource, Default, InspectorOptions)]
#[reflect(Resource, InspectorOptions)]
struct MapgridDebug {
    mouse_chunk: IVec2,
}

#[derive(Default, Resource)]
struct Chunks(HashMap<IVec2, Entity>);

const TILE_SIZE: Vec2 = Vec2::new(40., 20.);
const CHUNK_SIZE_TILES: UVec2 = UVec2::new(32, 32);
const CHUNK_SIZE: Vec2 = Vec2::new(
    TILE_SIZE.x * CHUNK_SIZE_TILES.x as f32,
    TILE_SIZE.y * CHUNK_SIZE_TILES.y as f32,
);

const AXONOMETRIC_PROJECTION: Mat2 = Mat2::from_cols(Vec2::new(0.5, -0.5), Vec2::new(0.5, 0.5));
const INV_AXONOMETRIC_PROJECTION: Mat2 = Mat2::from_cols(Vec2::new(1.0, 1.0), Vec2::new(-1.0, 1.0));

fn setup(mut cmd: Commands, assets: Res<AssetServer>) {
    let tex = assets.load("iso.png");
    cmd.insert_resource(Tileset {
        tex,
        water: 1,
        sand: 2,
    });

    cmd.insert_resource(WorldgenConfig {
        render_dist: 1,
        wavelength: 10.5,
        land_threshhold: 0.2,
    });

    cmd.insert_resource(TerrainNoise {
        simplex1: Simplex::new(1337),
    });
}

fn world2grid_chunk(world: Vec2) -> IVec2 {
    let normal = world / CHUNK_SIZE;
    let Vec2 { x, y } = INV_AXONOMETRIC_PROJECTION * normal;
    IVec2 {
        x: (x + 0.5).floor() as i32,
        y: (y + 0.5).floor() as i32,
    }
}

fn grid2world_chunk(grid: IVec2) -> Vec2 {
    // project then scale
    CHUNK_SIZE * (AXONOMETRIC_PROJECTION * grid.as_vec2())
}

fn debug_view(
    window: Query<&Window>,
    cam: Query<(&Camera, &GlobalTransform)>,
    mut dbg: ResMut<MapgridDebug>,
) {
    let window = window.single();
    let (cam, transform) = cam.single();

    if let Some(worldpos) = window.cursor_position().and_then(|cursor| {
        cam.viewport_to_world(transform, cursor)
            .map(|ray| ray.origin.truncate())
    }) {
        dbg.mouse_chunk = world2grid_chunk(worldpos);
    }
}

fn spawn_chunks(
    mut cmd: Commands,
    mut chunks: ResMut<Chunks>,
    mut mat: ResMut<Assets<Map>>,
    ts: Res<Tileset>,
    cfg: Res<WorldgenConfig>,
    noise: Res<TerrainNoise>,
    cam: Query<&Transform, With<Camera>>,
) {
    let transform = cam.single();
    let chunkpos = world2grid_chunk(transform.translation.xy());
    let render_dist = cfg.render_dist as i32;
    for y in (chunkpos.y - render_dist)..(chunkpos.y + render_dist) {
        for x in (chunkpos.x - render_dist)..(chunkpos.x + render_dist) {
            let vec = IVec2::new(x, y);
            if !chunks.0.contains_key(&vec) {
                debug!("spawning chunk @ {}, curr: {}", vec, chunkpos);
                let chunk = Map::builder(CHUNK_SIZE_TILES, ts.tex.clone(), TILE_SIZE)
                    .with_projection(AXONOMETRIC)
                    .build_and_initialize(|index| {
                        init_chunk(index, vec, noise.as_ref(), cfg.as_ref(), ts.as_ref())
                    });

                let pos = grid2world_chunk(vec);
                chunks.0.insert(
                    vec,
                    cmd.spawn(MapBundleManaged::new(chunk, mat.as_mut()))
                        .insert(Chunk)
                        .insert(Transform {
                            translation: Vec3::new(pos.x, pos.y, 0.),
                            ..default()
                        })
                        .id(),
                );
            }
        }
    }
}

fn manhatten_dist(a: IVec2, b: IVec2) -> u32 {
    let dist = (a - b).abs();
    (dist.x + dist.y).try_into().expect("negative distance?")
}

fn despawn_chunks(
    mut cmd: Commands,
    mut chunks: ResMut<Chunks>,
    maps: Query<&Transform, With<Chunk>>,
    cfg: Res<WorldgenConfig>,
    cam: Query<&Transform, With<Camera>>,
) {
    let cam_transform = cam.single();
    let render_dist = cfg.render_dist;
    for chunk_transform in maps.iter() {
        let chunkpos_cam = world2grid_chunk(cam_transform.translation.xy());
        let chunkpos_chunk = world2grid_chunk(chunk_transform.translation.xy());
        let dist = manhatten_dist(chunkpos_cam, chunkpos_chunk);
        if dist > render_dist * 2 {
            let e = chunks
                .0
                .remove(&chunkpos_chunk)
                .expect("tried to despawn nonexistant chunk");
            cmd.entity(e).despawn_recursive();
        }
    }
}

fn init_chunk(
    m: &mut MapIndexer,
    chunk: IVec2,
    noise: &TerrainNoise,
    cfg: &WorldgenConfig,
    tiles: &Tileset,
) {
    for y in 0..m.size().y {
        for x in 0..m.size().x {
            //m.set(x, y, (((x + y) % 2) + 1) as u32);
            let pos = IVec2::new(x as i32, y as i32);
            let global = chunk * pos;
            let val = noise.simplex1.get([
                global.x as f64 / cfg.wavelength,
                global.y as f64 / cfg.wavelength,
            ]);
            let tile = match val {
                x if x >= cfg.land_threshhold => tiles.sand,
                _ => tiles.water,
            };
            m.set(x, y, tile);
        }
    }
}
