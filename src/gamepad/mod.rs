use bevy::input::gamepad::{GamepadConnection, GamepadConnectionEvent};
use bevy::prelude::*;

use crate::controls::{PlayerShot, ShootDirection};
use crate::player::{Player, Velocity};

pub struct GamepadPlugin;

impl Plugin for GamepadPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, (gamepad_connections, gamepad_input));
    }
}

#[derive(Component)]
pub struct MyGamepad(Gamepad);

fn gamepad_connections(
    mut commands: Commands,
    mut gamepad_evr: EventReader<GamepadConnectionEvent>,
    gamepad_query: Query<(Entity, &MyGamepad)>,
) {
    for ev in gamepad_evr.read() {
        let id = ev.gamepad;
        match &ev.connection {
            GamepadConnection::Connected(info) => {
                info!(
                    "New gamepad connected with ID: {:?}, name: {}",
                    id, info.name
                );
                let gamepad_exists = gamepad_query.iter().any(|(_, gp)| gp.0 == id);
                if !gamepad_exists {
                    commands.spawn(MyGamepad(id));
                }
            }
            GamepadConnection::Disconnected => {
                info!("Lost gamepad connection with ID: {:?}", id);
                if let Some((ent, _)) = gamepad_query.iter().find(|(_, gp)| gp.0 == id) {
                    commands.entity(ent).despawn();
                }
            }
        }
    }
}

fn gamepad_input(
    axes: Res<Axis<GamepadAxis>>,
    buttons: Res<ButtonInput<GamepadButton>>,
    mut shoot: EventWriter<PlayerShot>,
    mut vel_query: Query<&mut Velocity, With<Player>>,
    gamepad_query: Query<&MyGamepad>,
) {
    let mut vel = vel_query.single_mut();

    for MyGamepad(gamepad) in gamepad_query.iter() {
        let axis_lx = GamepadAxis {
            gamepad: *gamepad,
            axis_type: GamepadAxisType::LeftStickX,
        };
        let axis_ly = GamepadAxis {
            gamepad: *gamepad,
            axis_type: GamepadAxisType::LeftStickY,
        };

        if let (Some(x), Some(y)) = (axes.get(axis_lx), axes.get(axis_ly)) {
            let left_stick_pos = Vec2::new(x, y);

            vel.0 = left_stick_pos;
            if vel.0.x != 0. && vel.0.y != 0. {
                info!(" x: {} y: {}", vel.0.x, vel.0.y);
            }
        }

        let sails_button = GamepadButton {
            gamepad: *gamepad,
            button_type: GamepadButtonType::South,
        };

        let shoot_button_left = GamepadButton {
            gamepad: *gamepad,
            button_type: GamepadButtonType::LeftTrigger,
        };

        let shoot_button_right = GamepadButton {
            gamepad: *gamepad,
            button_type: GamepadButtonType::RightTrigger,
        };

        if buttons.just_pressed(sails_button) {
            info!("Argggg! Sails be toggled!");
        }

        if buttons.just_pressed(shoot_button_left) {
            shoot.send(PlayerShot(ShootDirection::Left));
        }

        if buttons.just_pressed(shoot_button_right) {
            shoot.send(PlayerShot(ShootDirection::Right));
        }
    }
}
