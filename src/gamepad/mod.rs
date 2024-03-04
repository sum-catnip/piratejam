use bevy::prelude::*;
use bevy::input::gamepad::{GamepadConnection, GamepadConnectionEvent};

use crate::player::PlayerShip;

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
    gamepads_query: Query<(Entity, &MyGamepad)>,
) {
    for ev in gamepad_evr.read() {
        let id = ev.gamepad;
        match &ev.connection {
            GamepadConnection::Connected(info) => {
                info!("New gamepad connected with ID: {:?}, name: {}", id, info.name);
                commands.spawn(MyGamepad(id));
            }
            GamepadConnection::Disconnected => {
                info!("Lost gamepad connection with ID: {:?}", id);
                for (entity, gamepad) in gamepads_query.iter() {
                    if gamepad.0 == id {
                        commands.entity(entity).despawn();
                    }
                }
            }
            _ => {}  // Discard anything else
        }
    }
}

fn gamepad_input(
    axes: Res<Axis<GamepadAxis>>,
    buttons: Res<ButtonInput<GamepadButton>>,
    query_gamepad: Query<&MyGamepad>,
    mut query_ship: Query<&mut PlayerShip>,
) {
    if let Ok(my_gamepad) = query_gamepad.get_single() {
        let gamepad = my_gamepad.0;

        let axis_lx = GamepadAxis { gamepad, axis_type: GamepadAxisType::LeftStickX };
        let axis_ly = GamepadAxis { gamepad, axis_type: GamepadAxisType::LeftStickY };

        if let Ok(mut ship) = query_ship.get_single_mut() {
            if let (Some(x), Some(y)) = (axes.get(axis_lx), axes.get(axis_ly)) {
                let left_stick_pos = Vec2::new(x, y);
                ship.velocity = left_stick_pos;
                if ship.velocity.x != 0. && ship.velocity.y != 0. {
                    info!(" x: {} y: {}", ship.velocity.x, ship.velocity.y);
                }
            }

            let sails_button = GamepadButton { gamepad, button_type: GamepadButtonType::South };
            if buttons.just_pressed(sails_button) {
                ship.sails_up = !ship.sails_up;
            }
        }
    } else {
        info!("No gamepad connected");
    }
}