use crate::controls::{PlayerShot, ShootDirection};
use bevy::prelude::*;

pub struct PlayerPlugin;

impl Plugin for PlayerPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, spawn_player)
            .add_systems(Update, (move_entites, shoot));
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

const MOVEMENT_SCALE: f32 = 25.;

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
