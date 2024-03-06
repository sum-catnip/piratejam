use bevy::prelude::*;

use rand::seq::SliceRandom;
use rand::thread_rng;

use crate::{
    player::{DamageCooldown, Health, Size, SpawnLiving, Speed, Velocity},
    worldgen::{tile_at_pos, world2grid_tile, TerrainNoise, Tile, WorldgenConfig},
};

pub struct BaddiesPlugin;

#[derive(Component)]
pub struct BaddyShip;

#[derive(Component)]
pub struct NavigationCooldown(pub Timer);

#[derive(Bundle)]
pub struct BaddyBundle {
    marker: BaddyShip,
    vel: Velocity,
    speed: Speed,
    sheet: SpriteSheetBundle,
    health: Health,
    dmg_cooldown: DamageCooldown,
    nav_cooldown: NavigationCooldown,
    // this should endup in a seperate ship specific container once we have ship-hopping
    size: Size,
}

impl Plugin for BaddiesPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, (spawn_baddie))
            .add_systems(Update, (baddy_collision, avoid_islands));
    }
}

fn spawn_baddie(
    mut commands: Commands,
    assets: Res<AssetServer>,
    mut spawnevt: EventWriter<SpawnLiving>,
    mut layouts: ResMut<Assets<TextureAtlasLayout>>,
) {
    let tex: Handle<Image> = assets.load("ship1.png");
    let layout = TextureAtlasLayout::from_grid(Vec2::new(80., 60.), 5, 1, None, None);
    let atlas_layout = layouts.add(layout);
    let id = commands
        .spawn(BaddyBundle {
            marker: BaddyShip,
            sheet: SpriteSheetBundle {
                texture: tex,
                atlas: TextureAtlas {
                    layout: atlas_layout,
                    index: 1,
                },
                transform: Transform::from_xyz(100., 0., 1.),
                ..default()
            },
            vel: Velocity(Vec2::new(10., 10.)),
            speed: Speed(5.),
            health: Health {
                health: 100,
                max: 100,
            },
            dmg_cooldown: DamageCooldown(Timer::from_seconds(1., TimerMode::Repeating)),
            nav_cooldown: NavigationCooldown(Timer::from_seconds(1., TimerMode::Repeating)),
            size: Size(IVec2::new(80, 60)),
        })
        .id();

    spawnevt.send(SpawnLiving(id));
}

fn baddy_collision(
    terrain_noise: Res<TerrainNoise>,
    worldgencfg: Res<WorldgenConfig>,
    time: Res<Time>,
    mut all_baddies: Query<
        (&mut Velocity, &Transform, &mut Health, &mut DamageCooldown),
        With<BaddyShip>,
    >,
) {
    for (mut vel, btransform, mut hp, mut cooldown) in all_baddies.iter_mut() {
        cooldown.0.tick(time.delta());
        // Add this to kinda predict where it is gonna be in a second.
        let world_pos = btransform.translation.truncate() + vel.0;
        let grid_pos = world2grid_tile(world_pos);
        let tile = tile_at_pos(grid_pos, &terrain_noise, &worldgencfg);
        if matches!(tile, Tile::Sand | Tile::Sand2) {
            *vel = Velocity(Vec2::ZERO);
            info!("collision with terrain detected");
            if cooldown.0.just_finished() {
                // i dunno lets do 10 for now
                hp.health -= 10;
            }
        }
    }
}

/*
I want the baddies to have some amount of AI to start.

My idea is to have them point in a random direction. But I didn't want them to
look like they are just bouncing off of islands. I want them to seem like they
are roaming the seas adventuring. Things I want them to do:

* Not run into islands and die
* Smoothly sail around the sea exploring
* Pursue the player when nearby

Extra credit
* Stopping at islands to "explore"
* Get stranded either at sea or on an island
*/
fn avoid_islands(
    terrain_noise: Res<TerrainNoise>,
    worldgencfg: Res<WorldgenConfig>,
    time: Res<Time>,
    mut all_baddies: Query<(&mut Velocity, &Transform, &mut NavigationCooldown), With<BaddyShip>>,
) {
    for (mut vel, btransform, mut nav_cooldown) in all_baddies.iter_mut() {
        nav_cooldown.0.tick(time.delta());
        if nav_cooldown.0.just_finished() {
            let world_pos = btransform.translation.truncate() + vel.0;
            let grid_pos = world2grid_tile(world_pos);
            let neighbors_grid_pos = get_neighbors(grid_pos);
            let mut safeTiles = neighbors_grid_pos.clone();
            // Retain only safe tiles
            safeTiles.retain(|&neighbor_pos| {
                let tile = tile_at_pos(neighbor_pos, &terrain_noise, &worldgencfg);
                !matches!(tile, Tile::Sand | Tile::Sand2) // Keep the tile if it's not Sand or Sand2
            });
            let mut rng = thread_rng();
            if let Some(random_neighbor) = safeTiles.choose(&mut rng) {
                info!("Random Neighbor Found: {}", random_neighbor);
                // todo: this is messing things up and making the enemy ship ZOOOOOOM
                vel.0 = random_neighbor.as_vec2();
            }
        }
    }
}

fn get_neighbors(grid_pos: IVec2) -> Vec<IVec2> {
    let grid_size = 6; // The grid size around the center to return
    let mut neighbors: Vec<IVec2> = Vec::new();
    let tilesize = IVec2::new(40, 20);
    let half_grid_size = grid_size / 2;

    for dx in -half_grid_size..=half_grid_size {
        for dy in -half_grid_size..=half_grid_size {
            if dx == 0 && dy == 0 {
                continue;
            }

            neighbors.push(IVec2 {
                x: grid_pos.x + (dx * tilesize.x),
                y: grid_pos.y + (dy * tilesize.y),
            });
        }
    }
    return neighbors;
}
