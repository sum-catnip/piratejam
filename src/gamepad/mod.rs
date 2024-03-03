use bevy::prelude::*;
use bevy::input::gamepad::{GamepadConnection, GamepadConnectionEvent};

pub struct GamepadPlugin;

impl Plugin for GamepadPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, (gamepad_connections, gamepad_input));
    }
}

#[derive(Resource)]
pub struct MyGamepad(Gamepad);

fn gamepad_connections(
    mut commands: Commands,
    my_gamepad: Option<Res<MyGamepad>>,
    mut gamepad_evr: EventReader<GamepadConnectionEvent>,
) {
    for ev in gamepad_evr.read() {
        let id = ev.gamepad;
        match &ev.connection {
            GamepadConnection::Connected(info) => {
                info!("New gamepad connected with ID: {:?}, name: {}", id, info.name);
                if my_gamepad.is_none() {
                    commands.insert_resource(MyGamepad(id));
                }
            }
            GamepadConnection::Disconnected => {
                info!("Lost gamepad connection with ID: {:?}", id);
                if let Some(MyGamepad(old_id)) = my_gamepad.as_deref() {
                    if *old_id == id {
                        commands.remove_resource::<MyGamepad>();
                    }
                }
            }
            // other events are irrelevant
            _ => {}
        }
    }
}

fn gamepad_input(
    axes: Res<Axis<GamepadAxis>>,
    buttons: Res<Input<GamepadButton>>,
    my_gamepad: Option<Res<MyGamepad>>,
    mut query: Query<&mut Transform, With<PirateShip>>,
) {
    let gamepad = if let Some(gp) = my_gamepad {
        gp.0
    } else {
        info!("No gamepad connected");
        return;
    };

    let axis_lx = GamepadAxis {
        gamepad, axis_type: GamepadAxisType::LeftStickX
    };
    let axis_ly = GamepadAxis {
        gamepad, axis_type: GamepadAxisType::LeftStickY
    };

    if let (Some(x), Some(y)) = (axes.get(axis_lx), axes.get(axis_ly)) {
        let left_stick_pos = Vec2::new(x, y);

        if left_stick_pos.length() > 0.9 && left_stick_pos.y != 0. && left_stick_pos.x != 0. {
            info!("{:?} x: {} y: {}", gamepad, left_stick_pos.x, left_stick_pos.y);
        }
    }

    let sails_button = GamepadButton {
        gamepad, button_type: GamepadButtonType::South
    };

    if buttons.just_pressed(sails_button) {
        
    }
}