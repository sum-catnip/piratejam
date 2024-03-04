use bevy::prelude::*;

pub struct PlayerPlugin;

// Rotation speed in radians per frame.
const SHIP_ROTATION_SPEED: f32 = 0.005;

impl Plugin for PlayerPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, spawn_player)
            .add_systems(Update, (move_player, toggle_sails));
    }
}

#[derive(Component)]
pub struct PlayerShip {
    pub velocity: Vec2,
    pub sails_up: bool,
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
            velocity: Vec2::ZERO,
            sails_up: true,
        }
    ));
}

const MOVEMENT_SCALE: f32 = 25.;

fn move_player(
    mut ships: Query<(&mut Transform, &PlayerShip)>,
    time: Res<Time>,
) {
    for (mut transform, ship) in &mut ships {
        let movement = (ship.velocity * MOVEMENT_SCALE) * time.delta_seconds();
        if ship.velocity.x < 0.0 {
            transform.scale.x = transform.scale.x.abs() * -1.;
        } else if ship.velocity.x > 0.0 {
            transform.scale.x = transform.scale.x.abs();
        }
        transform.translation.x += movement.x;
        transform.translation.y += movement.y;
    }
}

fn toggle_sails(
    mut ships: Query<(&mut Transform, &mut PlayerShip)>,
    time: Res<Time>,
) {
    for (mut transform, mut ship) in &mut ships {
        if ship.sails_up && ship.velocity != Vec2::ZERO {
            ship.velocity = Vec2::ZERO;
        } else if !ship.sails_up {
            info!("Sails Down!");
            ship.velocity = Vec2::from_angle(SHIP_ROTATION_SPEED);
            let movement = (ship.velocity * (MOVEMENT_SCALE/10.)) * time.delta_seconds();
            // TODO(kronsby): Get windspeed and direction here and set the values to that
            transform.translation.x += movement.x;
            transform.translation.y += movement.y;
        }
    }
    
}