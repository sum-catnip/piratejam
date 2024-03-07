use std::collections::HashMap;
use std::num::Wrapping as wrap;

use bevy::{
    math::{ivec2, vec2},
    prelude::*,
};
use bevy_fast_tilemap::{FastTileMapPlugin, Map, MapBundleManaged, MapIndexer, AXONOMETRIC};
use bevy_inspector_egui::{prelude::*, quick::ResourceInspectorPlugin};

use num_enum::{IntoPrimitive, TryFromPrimitive};

use noise::{NoiseFn, Simplex, SuperSimplex, Value};

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

pub struct TileAssembly {
    /// maps tile position relative to origin
    /// onto tile index relative to first tile
    tiles: Vec<(IVec2, u32)>
}

#[derive(Resource)]
pub struct TerrainFeatures {
    palm: TileAssembly,
    stone: TileAssembly,
    pebble: TileAssembly
}

#[repr(u32)]
#[derive(Default, Reflect, IntoPrimitive, TryFromPrimitive)]
pub enum FeatureTile {
    #[default]
    PalmOrigin = 0,
    StoneOrigin = 7,
    PebbleOrigin = 10,
}

// art department:
// ⬇️ ↙️ ⬅️ ↖️ ⬆️ ↗️ ➡️ ↘️  and added the inner corners at the end ⬇️ ⬅️ ⬆️ ➡️
#[repr(u32)]
#[derive(Default, Reflect, IntoPrimitive, TryFromPrimitive)]
pub enum TerrainTile {
    #[default]
    Water = 0,
    Sand1 = 1,
    Sand2 = 2,
    IslandBorder1Bottom = 3,
    IslandBorder1BottomLeft = 4,
    IslandBorder1Left = 5,
    IslandBorder1TopLeft = 6,
    IslandBorder1Top = 7,
    IslandBorder1TopRight = 8,
    IslandBorder1Right = 9,
    IslandBorder1BottomRight = 10,
    IslandBorder1InnerDown = 11,
    IslandBorder1InnerLeft = 12,
    IslandBorder1InnerUp = 13,
    IslandBorder1InnerRight = 14,
    IslandBorder2Bottom = 15,
    IslandBorder2BottomLeft = 16,
    IslandBorder2Left = 17,
    IslandBorder2TopLeft = 18,
    IslandBorder2Top = 19,
    IslandBorder2TopRight = 20,
    IslandBorder2Right = 21,
    IslandBorder2BottomRight = 22,
    IslandBorder2InnerBottom = 23,
    IslandBorder2InnerLeft = 24,
    IslandBorder2InnerTop = 25,
    IslandBorder2InnerRight = 26,
}

#[repr(u32)]
#[derive(IntoPrimitive, TryFromPrimitive)]
enum RelativeBorderTile {
    IslandBorderBottom = 0,
    IslandBorderBottomLeft = 1,
    IslandBorderLeft = 2,
    IslandBorderTopLeft = 3,
    IslandBorderTop = 4,
    IslandBorderTopRight = 5,
    IslandBorderRight = 6,
    IslandBorderBottomRight = 7,
    IslandBorderInnerBottom = 8,
    IslandBorderInnerLeft = 9,
    IslandBorderInnerTop = 10,
    IslandBorderInnerRight = 11,
}

#[repr(u32)]
#[derive(Default, Reflect, IntoPrimitive, TryFromPrimitive)]
pub enum Biome {
    #[default]
    Ocean,
    Island,
}

#[derive(Resource)]
struct Tilesets {
    terrain: Handle<Image>,
    features: Handle<Image>,
    debug: Handle<Image>,
}

#[derive(Resource)]
pub struct TerrainNoise {
    heightmap: SuperSimplex,
    scatter: Value,
    sand: Simplex,
}

#[derive(Component)]
struct Chunk;

#[derive(Reflect, Resource, Default, InspectorOptions)]
#[reflect(Resource, InspectorOptions)]
pub struct WorldgenConfig {
    render_dist: u32,
    wavelength: f64,
    island_threshhold: f64,
    sand_freq: f64,
    palm_threshhold: f64,
    stone_threshhold: f64,
}

#[derive(Reflect, Resource, Default, InspectorOptions)]
#[reflect(Resource, InspectorOptions)]
struct MapgridDebug {
    mouse_chunk: IVec2,
    mouse_tile: IVec2,
    mouse_tileid: TerrainTile,
    mouse_biome: Biome,
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
    let terrain = assets.load("terrain.png");
    let features = assets.load("features.png");
    let debug = assets.load("iso.png");
    cmd.insert_resource(Tilesets {
        terrain,
        features,
        debug,
    });

    cmd.insert_resource(WorldgenConfig {
        render_dist: 5,
        wavelength: 200.,
        island_threshhold: 0.9,
        sand_freq: 10000.,
        stone_threshhold: 0.7,
        palm_threshhold: 0.7,
    });

    cmd.insert_resource(TerrainNoise {
        heightmap: SuperSimplex::new(1337),
        sand: Simplex::new(1337),
        scatter: Value::new(1337),
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
        dbg.mouse_tileid = noise.tile_at_pos(dbg.mouse_tile, cfg.as_ref());
        dbg.mouse_biome = noise.biome_at_pos(dbg.mouse_tile, cfg.as_ref());
    }
}

fn spawn_chunk(
    cmd: &mut Commands,
    mat: &mut Assets<Map>,
    ts: &Tilesets,
    cfg: &WorldgenConfig,
    noise: &TerrainNoise,
    chunkpos: IVec2,
) -> Entity {
    let pos = grid2world_chunk(chunkpos);
    let chunk_terrain = Map::builder(CHUNK_SIZE_TILES, ts.terrain.clone(), TILE_SIZE)
        .with_projection(AXONOMETRIC)
        .build_and_initialize(|index| init_chunk_terrain(index, chunkpos, noise, cfg));

    /*
    let chunk_debug = Map::builder(CHUNK_SIZE_TILES, ts.debug.clone(), TILE_SIZE)
        .with_projection(AXONOMETRIC)
        .build_and_set(|_| 0);

    _ = cmd.spawn(MapBundleManaged::new(chunk_debug, mat))
        .insert(Transform {
            translation: Vec3::new(pos.x, pos.y, 0.),
            ..default()
        })
        .id();
    */
    cmd.spawn(MapBundleManaged::new(chunk_terrain, mat))
        .insert(Chunk)
        .insert(Transform {
            translation: Vec3::new(pos.x, pos.y, -100.),
            ..default()
        })
        .id()
}

fn spawn_chunks(
    mut cmd: Commands,
    mut chunks: ResMut<Chunks>,
    mut mat: ResMut<Assets<Map>>,
    ts: Res<Tilesets>,
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
fn normalize_noise(noise: f64) -> f64 {
    noise / 2. + 0.5
}

// https://stackoverflow.com/a/37221804
fn cash_rng(x: i32, y: i32) -> i32 {
    let seed = wrap(1337);
    let x = wrap(x);
    let y = wrap(y);
    //all constants are prime
    let mut h = seed + x * wrap(374761393) + y * wrap(668265263);
    h = (h ^ (h >> 13)) * wrap(1274126177);
    (h ^ (h >> 16)).0
}

impl TerrainNoise {
    fn get_height(&self, pos: IVec2, cfg: &WorldgenConfig) -> f64 {
        normalize_noise(
            self.heightmap
                .get([pos.x as f64 / cfg.wavelength, pos.y as f64 / cfg.wavelength]),
        )
    }

    fn get_random(&self, pos: IVec2) -> f64 {
        let r = cash_rng(pos.x, pos.y);
        let f = r as f64 / i32::MAX as f64;
        debug!("r: {}, f: {}", r, f);
        f
    }

    pub fn tile_at_pos(&self, pos: IVec2, cfg: &WorldgenConfig) -> TerrainTile {
        match self.biome_at_pos(pos, cfg) {
            Biome::Ocean => TerrainTile::Water,
            Biome::Island => self.generate_island(pos, cfg),
        }
    }

    pub fn feature_at_pos(&self, pos: IVec2, cfg: &WorldgenConfig) -> TerrainTile {
        match self.biome_at_pos(pos, cfg) {
            Biome::Ocean => TerrainTile::Water,
            Biome::Island => self.generate_island(pos, cfg),
        }
    }

    pub fn biome_at_pos(&self, pos: IVec2, cfg: &WorldgenConfig) -> Biome {
        match self.get_height(pos, &cfg) {
            x if x >= cfg.island_threshhold => Biome::Island,
            _ => Biome::Ocean,
        }
    }

    fn in_ocean(&self, pos: IVec2, cfg: &WorldgenConfig) -> bool {
        matches!(self.biome_at_pos(pos, cfg), Biome::Ocean)
    }

    fn generate_island(&self, pos: IVec2, cfg: &WorldgenConfig) -> TerrainTile {
        // generate sand variation
        let min = TerrainTile::Sand1 as u32;
        let max = TerrainTile::Sand2 as u32;
        let variations = max - min + 1;
        let r = self.get_random(pos);
        // variant relative to first tile
        let vx = variations as f64 * r;
        let variant = (vx.trunc() as u32).min(variations - 1);
        debug!("variant: {}", variant);
        let variant_edges = match variant {
            0 => TerrainTile::IslandBorder1Bottom,
            1 => TerrainTile::IslandBorder2Bottom,
            _ => unreachable!(),
        } as u32;

        TerrainTile::try_from_primitive(
            if self.in_ocean(pos + ivec2(1, 0), cfg) && self.in_ocean(pos + ivec2(0, -1), cfg) {
                variant_edges + RelativeBorderTile::IslandBorderBottom as u32
            } else if self.in_ocean(pos + ivec2(-1, 0), cfg)
                && self.in_ocean(pos + ivec2(0, -1), cfg)
            {
                variant_edges + RelativeBorderTile::IslandBorderLeft as u32
            } else if self.in_ocean(pos + ivec2(-1, 0), cfg)
                && self.in_ocean(pos + ivec2(0, 1), cfg)
            {
                variant_edges + RelativeBorderTile::IslandBorderTop as u32
            } else if self.in_ocean(pos + ivec2(1, 0), cfg) && self.in_ocean(pos + ivec2(0, 1), cfg)
            {
                variant_edges + RelativeBorderTile::IslandBorderRight as u32
            } else if self.in_ocean(pos + ivec2(1, 0), cfg) {
                variant_edges + RelativeBorderTile::IslandBorderBottomRight as u32
            } else if self.in_ocean(pos + ivec2(-1, 0), cfg) {
                variant_edges + RelativeBorderTile::IslandBorderTopLeft as u32
            } else if self.in_ocean(pos + ivec2(0, -1), cfg) {
                variant_edges + RelativeBorderTile::IslandBorderBottomLeft as u32
            } else if self.in_ocean(pos + ivec2(0, 1), cfg) {
                variant_edges + RelativeBorderTile::IslandBorderTopRight as u32
            } else if self.in_ocean(pos + ivec2(-1, -1), cfg) {
                variant_edges + RelativeBorderTile::IslandBorderInnerLeft as u32
            } else if self.in_ocean(pos + ivec2(-1, 1), cfg) {
                variant_edges + RelativeBorderTile::IslandBorderInnerTop as u32
            } else if self.in_ocean(pos + ivec2(1, 1), cfg) {
                variant_edges + RelativeBorderTile::IslandBorderInnerRight as u32
            } else if self.in_ocean(pos + ivec2(1, -1), cfg) {
                variant_edges + RelativeBorderTile::IslandBorderInnerBottom as u32
            } else {
                min + variant
            },
        )
        .unwrap()
    }
}

fn init_chunk_terrain(m: &mut MapIndexer, chunk: IVec2, noise: &TerrainNoise, cfg: &WorldgenConfig) {
    for y in 0..m.size().y {
        for x in 0..m.size().x {
            let mut pos = IVec2::new(x as i32, y as i32);
            // global position
            pos += chunk * CHUNK_SIZE_TILES.as_ivec2();
            m.set(x, y, noise.tile_at_pos(pos, cfg) as u32);
        }
    }
}
