use bevy::prelude::*;

pub struct PlayerPlugin;

impl Plugin for PlayerPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, spawn_player);
    }
}

#[derive(Component)]
pub struct PlayerShip {
    pub velocity: Vec3,
}

fn spawn_player(
    mut commands: Commands,
    assets: Res<AssetServer>
) {
    let sprite_size = Vec2::new(32.0, 32.0);
    commands.spawn((
        SpriteBundle {
            texture: assets.load("ducky.png"),
            transform: Transform::from_scale(
                Vec3::new(sprite_size.x / 200., sprite_size.y / 225., 1.)),
            ..default()
        },
        PlayerShip{
            velocity: Vec3::ZERO,
        }
    ));
}