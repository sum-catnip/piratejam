use crate::{
    controls::{PlayerShot, ShootDirection},
    worldgen::{world2grid_tile, TerrainNoise, TerrainTile, WorldgenConfig},
};
use bevy::prelude::*;

pub struct PlayerPlugin;

impl Plugin for PlayerPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, spawn_player)
            .add_systems(
                Update,
                (move_entities, shoot, update_sprite, tile_collision),
            )
            .add_event::<SpawnLiving>();
    }
}

#[derive(Event)]
pub struct SpawnLiving(pub Entity);

#[derive(Component)]
pub struct Velocity(pub Vec2);

#[derive(Component)]
pub struct DamageCooldown(pub Timer);

#[derive(Component)]
pub struct Health {
    pub health: u32,
    pub max: u32,
}

// size of the art in pixels
#[derive(Component)]
pub struct Size(pub IVec2);

// todo: remove speed from move_entities function
// use it only to initialize velocity, as velocity is speed and direction
#[derive(Component)]
pub struct Speed(pub f32);

#[derive(Component)]
pub struct Player;

#[derive(Bundle)]
pub struct PlayerBundle {
    marker: Player,
    vel: Velocity,
    speed: Speed,
    sheet: SpriteSheetBundle,
    health: Health,
    dmg_cooldown: DamageCooldown,
    // this should endup in a seperate ship specific container once we have ship-hopping
    size: Size,
}

#[derive(Bundle)]
pub struct CannonballBundle {
    vel: Velocity,
    speed: Speed,
    sprite: SpriteBundle,
}

fn spawn_player(
    mut commands: Commands,
    assets: Res<AssetServer>,
    mut spawnevt: EventWriter<SpawnLiving>,
    mut layouts: ResMut<Assets<TextureAtlasLayout>>,
) {
    let tex: Handle<Image> = assets.load("boat.png");
    let layout = TextureAtlasLayout::from_grid(Vec2::new(32., 32.), 5, 1, None, None);
    let atlas_layout = layouts.add(layout);
    let id = commands
        .spawn(PlayerBundle {
            marker: Player,
            sheet: SpriteSheetBundle {
                texture: tex,
                atlas: TextureAtlas {
                    layout: atlas_layout,
                    index: 1,
                },
                transform: Transform::from_xyz(0., 0., 1.),
                ..default()
            },
            vel: Velocity(Vec2::ZERO),
            speed: Speed(35.),
            health: Health {
                health: 100,
                max: 100,
            },
            dmg_cooldown: DamageCooldown(Timer::from_seconds(1., TimerMode::Repeating)),
            size: Size(IVec2::new(32, 32)),
        })
        .id();

    spawnevt.send(SpawnLiving(id));
}

fn move_entities(mut ships: Query<(&mut Transform, &Velocity, &Speed, Entity)>, time: Res<Time>) {
    for (mut transform, velocity, speed, entity) in &mut ships {
        info!("moving: {:?}, speed: {}", entity, speed.0);
        let movement = (velocity.0 * speed.0) * time.delta_seconds();
        if velocity.0.x < 0.0 {
            transform.scale.x = transform.scale.x.abs() * -1.;
        } else if velocity.0.x > 0.0 {
            transform.scale.x = transform.scale.x.abs();
        }
        transform.translation.x += movement.x;
        transform.translation.y += movement.y;
    }
}

fn shoot(
    mut cmd: Commands,
    mut shot: EventReader<PlayerShot>,
    player: Query<&Transform, With<Player>>,
    assets: Res<AssetServer>,
) {
    let ptransform = player.single();
    for e in shot.read() {
        debug!("shooting {:?}", e.0);
        let vel = match e.0 {
            ShootDirection::Left => ptransform.left().xy(),
            ShootDirection::Right => ptransform.right().xy(),
        };

        cmd.spawn(CannonballBundle {
            vel: Velocity(vel),
            speed: Speed(70.),
            sprite: SpriteBundle {
                texture: assets.load("cannonball.png"),
                transform: Transform::from_translation(ptransform.translation),
                ..default()
            },
        });
    }
}

fn update_sprite(mut query: Query<(&Velocity, &mut Sprite, &mut TextureAtlas)>) {
    for (vel, mut sprite, mut atlas) in query.iter_mut() {
        let direction = vel.0.normalize_or_zero();
        let angle = direction.y.atan2(direction.x);

        let (index, flip) =
            if (angle > (-std::f32::consts::PI / 8.0)) && (angle <= (std::f32::consts::PI / 8.0)) {
                (2, true) // Right
            } else if (angle > (std::f32::consts::PI / 8.0))
                && (angle <= ((3.0 * std::f32::consts::PI) / 8.0))
            {
                (0, true) // Up Right
            } else if (angle > (3.0 * std::f32::consts::PI) / 8.0)
                && (angle <= ((5.0 * std::f32::consts::PI) / 8.0))
            {
                (1, false) // Up
            } else if (angle > (5.0 * std::f32::consts::PI) / 8.0)
                && (angle <= ((7.0 * std::f32::consts::PI) / 8.0))
            {
                (0, false) // Up Left
            } else if (angle <= (-std::f32::consts::PI / 8.0))
                && (angle > ((-3.0 * std::f32::consts::PI) / 8.0))
            {
                (3, true) // Down Right
            } else if (angle <= ((-3.0 * std::f32::consts::PI) / 8.0))
                && (angle > ((-5.0 * std::f32::consts::PI) / 8.0))
            {
                (4, false) // Down
            } else if (angle <= ((-5.0 * std::f32::consts::PI) / 8.0))
                && (angle > ((-7.0 * std::f32::consts::PI) / 8.0))
            {
                (3, false) // Down Left
            } else {
                (2, false) // Left
            };

        atlas.index = index;

        if flip {
            sprite.flip_x = flip;
        }
    }
}

fn tile_collision(
    terrain_noise: Res<TerrainNoise>,
    worldgencfg: Res<WorldgenConfig>,
    time: Res<Time>,
    mut ships: Query<(&mut Velocity, &Transform, &mut Health, &mut DamageCooldown)>,
) {
    for (mut vel, ptransform, mut hp, mut cooldown) in ships.iter_mut() {
        cooldown.0.tick(time.delta());
        // Add this to kinda predict where it is gonna be in a second.
        let world_pos = ptransform.translation.truncate() + vel.0;
        let grid_pos = world2grid_tile(world_pos);
        let tile = terrain_noise.tile_at_pos(grid_pos, &worldgencfg);
        if matches!(tile, TerrainTile::Sand1 | TerrainTile::Sand2) {
            *vel = Velocity(Vec2::ZERO);
            info!("collision with terrain detected");
            if cooldown.0.just_finished() && hp.health > 0 {
                // i dunno lets do 10 for now
                hp.health -= 10;
            }
        }
    }
}
