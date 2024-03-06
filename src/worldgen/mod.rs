use std::collections::HashMap;

use bevy::prelude::*;
use bevy_fast_tilemap::{FastTileMapPlugin, Map, MapBundleManaged, MapIndexer, AXONOMETRIC};
use bevy_inspector_egui::{prelude::*, quick::ResourceInspectorPlugin};

use num_enum::{IntoPrimitive, TryFromPrimitive};

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
            //.add_systems(Startup, spawn_test_chunk.after(setup))
            .add_systems(Update, debug_view);
    }
}

#[repr(u32)]
#[derive(Default, Reflect, IntoPrimitive, TryFromPrimitive)]
pub enum Tile {
    Sand = 0,
    Sand2 = 1,
    #[default]
    Water = 2,
}

#[derive(Resource)]
struct Tileset {
    tex: Handle<Image>,
}

#[derive(Resource)]
pub struct TerrainNoise {
    altitude: Simplex,
    sand: Simplex,
}

#[derive(Component)]
struct Chunk;

#[derive(Reflect, Resource, Default, InspectorOptions)]
#[reflect(Resource, InspectorOptions)]
pub struct WorldgenConfig {
    render_dist: u32,
    wavelength: f64,
    land_threshhold: f64,
    sand_freq: f64,
}

#[derive(Reflect, Resource, Default, InspectorOptions)]
#[reflect(Resource, InspectorOptions)]
struct MapgridDebug {
    mouse_chunk: IVec2,
    mouse_tile: IVec2,
    mouse_tileid: Tile,
}

#[derive(Default, Resource)]
struct Chunks(HashMap<IVec2, Entity>);

const TILE_SIZE: Vec2 = Vec2::new(40., 20.);
const CHUNK_SIZE_TILES: UVec2 = UVec2::new(25, 25);
const CHUNK_SIZE: Vec2 = Vec2::new(
    TILE_SIZE.x * CHUNK_SIZE_TILES.x as f32,
    TILE_SIZE.y * CHUNK_SIZE_TILES.y as f32,
);

const AXONOMETRIC_PROJECTION: Mat2 = Mat2::from_cols(Vec2::new(0.5, -0.5), Vec2::new(0.5, 0.5));
const INV_AXONOMETRIC_PROJECTION: Mat2 = Mat2::from_cols(Vec2::new(1.0, 1.0), Vec2::new(-1.0, 1.0));

fn setup(mut cmd: Commands, assets: Res<AssetServer>) {
    let tex = assets.load("tiles.png");
    cmd.insert_resource(Tileset { tex });

    cmd.insert_resource(WorldgenConfig {
        render_dist: 5,
        wavelength: 200.,
        land_threshhold: 0.65,
        sand_freq: 10000.,
    });

    cmd.insert_resource(TerrainNoise {
        altitude: Simplex::new(1337),
        sand: Simplex::new(1337),
    });
}

pub fn world2grid_tile(world: Vec2) -> IVec2 {
    let normal = world / TILE_SIZE;
    let Vec2 { x, y } = INV_AXONOMETRIC_PROJECTION * normal;
    IVec2 {
        x: (x + 0.5 + (CHUNK_SIZE_TILES.x / 2) as f32).floor() as i32,
        y: (y + 0.5 + (CHUNK_SIZE_TILES.y / 2) as f32).floor() as i32,
    }
}

pub fn grid2world_tile(grid: IVec2) -> Vec2 {
    // project then scale
    TILE_SIZE * (AXONOMETRIC_PROJECTION * grid.as_vec2())
}

pub fn world2grid_chunk(world: Vec2) -> IVec2 {
    let normal = world / CHUNK_SIZE;
    let Vec2 { x, y } = INV_AXONOMETRIC_PROJECTION * normal;
    IVec2 {
        x: (x + 0.5).floor() as i32,
        y: (y + 0.5).floor() as i32,
    }
}

pub fn grid2world_chunk(grid: IVec2) -> Vec2 {
    // project then scale
    CHUNK_SIZE * (AXONOMETRIC_PROJECTION * grid.as_vec2())
}

fn debug_view(
    window: Query<&Window>,
    cam: Query<(&Camera, &GlobalTransform)>,
    noise: Res<TerrainNoise>,
    cfg: Res<WorldgenConfig>,
    mut dbg: ResMut<MapgridDebug>,
) {
    let window = window.single();
    let (cam, transform) = cam.single();

    if let Some(worldpos) = window.cursor_position().and_then(|cursor| {
        cam.viewport_to_world(transform, cursor)
            .map(|ray| ray.origin.truncate())
    }) {
        dbg.mouse_chunk = world2grid_chunk(worldpos);
        dbg.mouse_tile = world2grid_tile(worldpos);
        dbg.mouse_tileid = tile_at_pos(dbg.mouse_tile, noise.as_ref(), cfg.as_ref())
    }
}

fn spawn_chunk(
    cmd: &mut Commands,
    mat: &mut Assets<Map>,
    ts: &Tileset,
    cfg: &WorldgenConfig,
    noise: &TerrainNoise,
    chunkpos: IVec2,
) -> Entity {
    let chunk = Map::builder(CHUNK_SIZE_TILES, ts.tex.clone(), TILE_SIZE)
        .with_projection(AXONOMETRIC)
        .build_and_initialize(|index| init_chunk(index, chunkpos, noise, cfg));

    let pos = grid2world_chunk(chunkpos);
    cmd.spawn(MapBundleManaged::new(chunk, mat))
        .insert(Chunk)
        .insert(Transform {
            translation: Vec3::new(pos.x, pos.y, -1.),
            ..default()
        })
        .id()
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
                chunks.0.insert(
                    vec,
                    spawn_chunk(
                        &mut cmd,
                        mat.as_mut(),
                        ts.as_ref(),
                        cfg.as_ref(),
                        noise.as_ref(),
                        vec,
                    ),
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
        if dist > render_dist * 2 + 1 {
            let e = chunks
                .0
                .remove(&chunkpos_chunk)
                .expect("tried to despawn nonexistant chunk");
            cmd.entity(e).despawn_recursive();
        }
    }
}

// normalize [-1, 1] to [0, 1]
fn normal_simplex(noise: f64) -> f64 {
    noise / 2. + 0.5
}

fn sand_noise(pos: IVec2, noise: &TerrainNoise, cfg: &WorldgenConfig) -> Tile {
    let val = normal_simplex(
        noise
            .sand
            .get([pos.x as f64 * cfg.sand_freq, pos.y as f64 * cfg.sand_freq]),
    );

    //let min = Tile::Sand as u32;
    //let max = Tile::Sand3 as u32;
    //let choice = (val * (max - min + 1) as f64 + min as f64).floor() as u32;
    //choice.try_into().unwrap()
    if val >= 0.6 {
        Tile::Sand2
    } else {
        Tile::Sand
    }
}

pub fn tile_at_pos(pos: IVec2, noise: &TerrainNoise, cfg: &WorldgenConfig) -> Tile {
    let val = normal_simplex(
        noise
            .altitude
            .get([pos.x as f64 / cfg.wavelength, pos.y as f64 / cfg.wavelength]),
    );
    match val {
        x if x >= cfg.land_threshhold => sand_noise(pos, noise, cfg),
        _ => Tile::Water,
    }
}

fn init_chunk(m: &mut MapIndexer, chunk: IVec2, noise: &TerrainNoise, cfg: &WorldgenConfig) {
    for y in 0..m.size().y {
        for x in 0..m.size().x {
            let mut pos = IVec2::new(x as i32, y as i32);
            // global position
            pos += chunk * CHUNK_SIZE_TILES.as_ivec2();
            m.set(x, y, tile_at_pos(pos, noise, cfg) as u32);
        }
    }
}
