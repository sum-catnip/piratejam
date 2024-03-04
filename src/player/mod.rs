use bevy::prelude::*;

pub struct PlayerPlugin;

impl Plugin for PlayerPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, spawn_player)
            .add_systems(Update, move_player);
    }
}

#[derive(Component)]
pub struct PlayerShip {
    pub velocity: Vec2,
}

fn spawn_player(mut commands: Commands, assets: Res<AssetServer>, mut layouts: ResMut<Assets<TextureAtlasLayout>>) {
    let tex: Handle<Image> = assets.load("boat.png");
    let layout = TextureAtlasLayout::from_grid(Vec2::new(32., 32.), 5, 1, None, None);
    let atlas_layout = layouts.add(layout);
    commands.spawn((
        SpriteSheetBundle {
            texture: tex,
            atlas: TextureAtlas {
                layout: atlas_layout,
                index: 1,
            },
            transform: Transform::from_xyz(0., 0., 1.),
            ..default()
        },
        PlayerShip {
            velocity: Vec2::ZERO,
        },
    ));
}

const MOVEMENT_SCALE: f32 = 25.;

fn move_player(mut ships: Query<(&mut Transform, &PlayerShip)>, time: Res<Time>) {
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

