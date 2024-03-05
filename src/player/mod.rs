use crate::{
    controls::{PlayerShot, ShootDirection},
    worldgen::{tile_at_pos, world2grid_tile, TerrainNoise, Tile, WorldgenConfig},
};
use bevy::prelude::*;

pub struct PlayerPlugin;

impl Plugin for PlayerPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, spawn_player)
            .add_systems(Update, (move_entites, shoot, update_sprite, tile_collision));
    }
}

#[derive(Component)]
pub struct Velocity(pub Vec2);

#[derive(Component)]
pub struct Player;

#[derive(Bundle)]
pub struct PlayerBundle {
    marker: Player,
    vel: Velocity,
    sheet: SpriteSheetBundle,
}

#[derive(Bundle)]
pub struct CannonballBundle {
    vel: Velocity,
    sprite: SpriteBundle,
}

fn spawn_player(
    mut commands: Commands,
    assets: Res<AssetServer>,
    mut layouts: ResMut<Assets<TextureAtlasLayout>>,
) {
    let tex: Handle<Image> = assets.load("boat.png");
    let layout = TextureAtlasLayout::from_grid(Vec2::new(32., 32.), 5, 1, None, None);
    let atlas_layout = layouts.add(layout);
    commands.spawn(PlayerBundle {
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
    });
}

const MOVEMENT_SCALE: f32 = 35.;

fn move_entites(mut ships: Query<(&mut Transform, &Velocity)>, time: Res<Time>) {
    for (mut transform, velocity) in &mut ships {
        let movement = (velocity.0 * MOVEMENT_SCALE) * time.delta_seconds();
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
    mut player: Query<(&mut Velocity, &Transform), With<Player>>,
) {
    let (mut vel, ptransform) = player.single_mut();
    let world_pos = ptransform.translation.truncate() + vel.0; // Add this to kinda predict where it is gonna be in a second.
    let grid_pos = world2grid_tile(world_pos);
    let tile = tile_at_pos(grid_pos, &terrain_noise, &worldgencfg);
    if matches!(tile, Tile::Sand | Tile::Sand2) {
        *vel = Velocity(Vec2::ZERO);
    }
}
