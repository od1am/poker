use bevy::prelude::*;
use bevy::pbr::*;

// Components
#[derive(Component)]
struct PokerTable;

#[derive(Component)]
struct Card {
    rank: super::Rank,
    suit: super::Suit,
    face_up: bool,
}

#[derive(Component)]
struct MainCamera;

// Resource for game state
#[derive(Resource)]
struct GameState {
    current_phase: GamePhase,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum GamePhase {
    Setup,
    PreFlop,
    Flop,
    Turn,
    River,
    Showdown,
}

pub struct PokerGamePlugin;

impl Plugin for PokerGamePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, setup)
            .add_systems(Update, (
                rotate_camera,
                // We'll add more systems here later
            ))
            .insert_resource(GameState {
                current_phase: GamePhase::Setup,
            });
    }
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    // Spawn the poker table
    commands.spawn((
        PbrBundle {
            mesh: meshes.add(shape::Box::new(4.0, 0.2, 2.5).into()),
            material: materials.add(Color::rgb(0.0, 0.4, 0.0).into()),
            transform: Transform::from_xyz(0.0, -0.1, 0.0),
            ..default()
        },
        PokerTable,
    ));

    // Add ambient light
    commands.spawn(PointLightBundle {
        point_light: PointLight {
            intensity: 1500.0,
            shadows_enabled: true,
            ..default()
        },
        transform: Transform::from_xyz(4.0, 8.0, 4.0),
        ..default()
    });

    // Add camera
    commands.spawn((
        Camera3dBundle {
            transform: Transform::from_xyz(-2.0, 2.5, 5.0)
                .looking_at(Vec3::ZERO, Vec3::Y),
            ..default()
        },
        MainCamera,
    ));
}

// Simple system to rotate the camera around the table
fn rotate_camera(
    time: Res<Time>,
    mut query: Query<&mut Transform, With<MainCamera>>,
) {
    for mut transform in &mut query {
        let rotation_speed = 0.1;
        transform.rotate_around(
            Vec3::ZERO,
            Quat::from_rotation_y(rotation_speed * time.delta_seconds()),
        );
    }
}

// We'll add more systems and components here as we build out the game 