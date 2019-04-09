use ggez::event::{self, EventHandler, MouseButton};
use ggez::graphics;
use ggez::nalgebra::{Point2, Vector2};
use ggez::{Context, ContextBuilder, GameResult};
use std::collections::HashMap;
use std::rc::Rc;

struct MyGame {
    focus_color: f32,
    focused: bool,
    selecting: Option<(Point2<f32>, Point2<f32>)>,
    units: Vec<Unit>,
    window_size: Vector2<f32>,
    game_map: GameMap,
    vehicle_image: graphics::Image,
    sand_image: graphics::Image,
    rock_image: graphics::Image,
}

struct GameMap {
    tiles: Vec<GroundTile>,
    width: usize,
    height: usize,
}

const NEIGHBOURS: [(i32, i32); 8] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];



const MAX_ELEVATION: f32 = -0.33;

impl GameMap {
    fn tile(&self, x: usize, y: usize) -> &GroundTile {
        &self.tiles[y * self.width + x]
    }

    fn can_reach_neighbour(&self, source_x: i32, source_y: i32, x_offset: i32, y_offset: i32) -> bool {
        let target_x = source_x + x_offset;
        let target_y = source_y + y_offset;

        // Check bounds
        if source_x < 0 || source_y < 0 || source_x >= self.width as i32 || source_y >= self.height as i32 {
            return false;
        }
        if target_x < 0 || target_y < 0 || target_x >= self.width as i32 || target_y >= self.height as i32 {
            return false;
        }

        // Bounds are good, guaranteed to fit in a usize
        let (source_x, target_x) = (source_x as usize, target_x as usize);
        let (source_y, target_y) = (source_y as usize, target_y as usize);

        // Consider x going directly (diagonally) to y; it can only reach if
        // the two 'o' are both free.
        // xo
        // oy
        let diagonally_okay = if x_offset != 0 && y_offset != 0 {
            self.tile(source_x, target_y).passable() && self.tile(target_x, source_y).passable()
        } else {
            true
        };

        diagonally_okay && self.tile(target_x, target_y).passable()
    }

    fn path_flow_to_target(&self, target: Point2<f32>) -> HashMap<(i32, i32), Vector2<f32>> {
        let (x, y) = ((target.x / 16.0) as i32, (target.y / 16.0) as i32);

        fn cost(x_offset: i32, y_offset: i32) -> u32 {
            match x_offset.abs() + y_offset.abs() {
                2 => 14, // ~= sqrt(2) * 10, diagonal of a square
                _ => 10, // 1 * 10
            }
        }

        let current_tile = self.to_tile(target);
        if !self.tile(current_tile.0, current_tile.1).passable() {
            return Default::default();
        }

        pathfinding::directed::dijkstra::dijkstra_all(&(x, y), |(x, y)| {
            NEIGHBOURS
                .iter()
                .filter(|(x_offset, y_offset)| {
                    self.can_reach_neighbour(*x, *y, *x_offset, *y_offset)
                })
                .map(|(x_offset, y_offset)| {
                    ((x + *x_offset, y + *y_offset), cost(*x_offset, *y_offset))
                })
                .collect::<Vec<_>>()
        })
        .into_iter()
        .map(|(position, (parent, _cost))| {
            let x_offset = position.0 - parent.0;
            let y_offset = position.1 - parent.1;

            // Can probably be const'd into a global map
            let direction = -Vector2::new(x_offset as f32, y_offset as f32).normalize();

            (position, direction)
        })
        .collect::<HashMap<_, _>>()
    }

    // TODO: Size
    fn raycast_to_target(&self, unit: &Unit, source: Point2<f32>, target: Point2<f32>) -> bool {
        let direction = (target - source).normalize();
        let step = direction * unit.speed();
        let mut position = source;
        let mut last_tile = self.to_tile(position);
        let steps = ((target - source).magnitude() / unit.speed()).floor() as usize;

        for _ in 0..steps {
            position += step;

            let new_tile = self.to_tile(position);

            if last_tile != new_tile {
                last_tile = new_tile;

                if !self.tile(new_tile.0, new_tile.1).passable() {
                    return false;
                }
            }
        }

        true
    }

    fn to_tile(&self, point: Point2<f32>) -> (usize, usize) {
        let (x, y) = ((point.x / 16.0).floor(), (point.y / 16.0).floor());

        let x = match x {
            i if i < 0.0 => 0,
            i if i >= self.width as f32 => self.width - 1,
            i => i as usize,
        };

        let y = match y {
            i if i < 0.0 => 0,
            i if i >= self.height as f32 => self.height - 1,
            i => i as usize,
        };

        (x, y)
    }
}

#[derive(Default, Clone)]
struct GroundTile {
    elevation: f32,
    /// The maximum unit size that can pass through this tile.
    /// 0 is impassable, 1 means the tile is free, 2 means this tile and all
    /// its neighbours are free, and so on.
    size: u8,
}

impl GroundTile {
    fn passable(&self) -> bool {
        self.elevation <= MAX_ELEVATION
    }
}

impl MyGame {
    fn new(ctx: &mut Context) -> GameResult<Self> {
        let vehicle_image = graphics::Image::new(ctx, "/vehicle.png")?;
        let sand_image = graphics::Image::new(ctx, "/sand.png")?;
        let rock_image = graphics::Image::new(ctx, "/rock.png")?;

        use noise::{Billow, NoiseFn};
        let billow = Billow::new();

        let mut game_map = GameMap {
            width: 100,
            height: 100,
            tiles: vec![GroundTile::default(); 100 * 100],
        };

        for y in 0..game_map.height {
            for x in 0..game_map.width {
                game_map.tiles[y * game_map.width + x].elevation =
                    billow.get([x as f64 / 100.0, y as f64 / 100.0]) as f32;
            }
        }

        Ok(Self {
            focus_color: 0.2,
            focused: true,
            selecting: None,
            units: Vec::default(),
            window_size: Vector2::new(0.0, 0.0),
            game_map,
            vehicle_image,
            sand_image,
            rock_image,
        })
    }
}

struct Unit {
    position: Point2<f32>,
    rotation: Vector2<f32>,
    intended_direction: Option<Vector2<f32>>,
    target: Option<Point2<f32>>,
    path: Option<Rc<HashMap<(i32, i32), Vector2<f32>>>>,
    selected: bool,
}

impl Unit {
    fn selected_by(&self, point: Point2<f32>, size: Vector2<f32>) -> bool {
        is_between(self.position, (point, point + size))
    }

    fn speed(&self) -> f32 {
        5.0
    }
}

fn to_rect(p1: Point2<f32>, p2: Point2<f32>) -> (Point2<f32>, Vector2<f32>) {
    let rect_p1 = Point2::new(p1.x.min(p2.x), p1.y.min(p2.y));
    let rect_p2 = Vector2::new((p2.x - p1.x).abs(), (p2.y - p1.y).abs());

    (rect_p1, rect_p2)
}

fn to_angle(vector: Vector2<f32>) -> f32 {
    vector.y.atan2(vector.x) + (std::f32::consts::PI / 2.0)
}

fn is_between(p: Point2<f32>, (p1, p2): (Point2<f32>, Point2<f32>)) -> bool {
    p.x >= p1.x && p.x <= p2.x && p.y >= p1.y && p.y <= p2.y
}

fn draw_circle(
    ctx: &mut Context,
    position: Point2<f32>,
    radius: f32,
    color: graphics::Color,
    fill: bool,
) -> GameResult<()> {
    let mesh = graphics::Mesh::new_circle(
        ctx,
        if fill {
            graphics::DrawMode::fill()
        } else {
            graphics::DrawMode::stroke(1.0)
        },
        position,
        radius,
        2.0,
        color,
    )?;
    graphics::draw(ctx, &mesh, graphics::DrawParam::default())?;

    Ok(())
}

fn interpolate_flow(flow: &HashMap<(i32, i32), Vector2<f32>>, point: Point2<f32>) -> Option<&Vector2<f32>> {
    let current_tile = point / 16.0;
    let current_tile = Point2::new(current_tile.x.floor(), current_tile.y.floor());
//    let position_within_tile = point - current_tile;
//
//    let neighbour_contribution = [f32; NEIGHBOURS.len()];
//
//    for (i, (x, y)) in NEIGHBOURS.iter().enumerate() {
//
//    }
//    NEIGHBOURS.iter()
//        .map(|(x_offset, y_offset)| (x_offset, y_offset, ))
//        .map(|offset| ...);

    flow.get(&(current_tile.x as i32, current_tile.y as i32))
}

impl EventHandler for MyGame {
    fn update(&mut self, _ctx: &mut Context) -> GameResult<()> {
        if self.focused && self.focus_color < 0.3 {
            self.focus_color += 0.01;
        } else if !self.focused && self.focus_color > 0.2 {
            self.focus_color -= 0.01;
        }

        for unit in self.units.iter_mut() {
            if let (Some(target), Some(flow)) = (&unit.target, &unit.path) {
                // Check direct line of sight, otherwise use the pre-computed Dijkstra flow
                let direction = if self.game_map.raycast_to_target(unit, unit.position, *target) {
                    (*target - unit.position).normalize()
                } else {
                    match interpolate_flow(flow.as_ref(), unit.position) {
                        Some(x) => x.clone(),
                        None => {
                            unit.intended_direction = None;
                            unit.target = None;
                            unit.path = None;
                            continue
                        },
                    }
                };

                // Check for non-permanent obstacles (other units)
                // ???

                // Move.
                let path = target - unit.position;
                if path.norm() > 10.0 {
                    unit.intended_direction = Some(direction);
                } else {
                    unit.intended_direction = None;
                    unit.target = None;
                    unit.path = None;
                }
            }
        }

        for unit in self.units.iter_mut() {
            if let Some(direction) = unit.intended_direction {
                unit.rotation = direction;
                unit.position += direction * unit.speed();
            }
        }

        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult<()> {
        graphics::clear(ctx, (0.2, 0.2, self.focus_color).into());

        // Draw ground
        let mut sand = graphics::spritebatch::SpriteBatch::new(self.sand_image.clone());
        let mut rocks = graphics::spritebatch::SpriteBatch::new(self.rock_image.clone());

        for x in 0..((self.window_size.x / 16.0) as usize) {
            for y in 0..((self.window_size.y / 16.0) as usize) {
                let draw_param = graphics::DrawParam::new()
                    .scale(Vector2::new(0.5, 0.5))
                    .dest(Point2::new(x as f32 * 16.0, y as f32 * 16.0));

                if self.game_map.tile(x, y).passable() {
                    sand.add(draw_param);
                } else {
                    rocks.add(draw_param);
                }
            }
        }

        graphics::draw(ctx, &sand, graphics::DrawParam::default())?;
        graphics::draw(ctx, &rocks, graphics::DrawParam::default())?;

        // Draw units
        for unit in self.units.iter() {
            let draw_param = graphics::DrawParam::new()
                .offset(Point2::new(0.5, 0.5))
                .rotation(to_angle(unit.rotation))
                .scale(Vector2::new(0.8, 0.8))
                .dest(unit.position);
            graphics::draw(ctx, &self.vehicle_image, draw_param)?;

            if unit.selected {
                draw_circle(ctx, unit.position, 20.0, (0.2, 0.2, 0.6).into(), false)?;
            }

            if let Some(target) = unit.target {
                let mesh = graphics::Mesh::new_line(
                    ctx,
                    &[unit.position, target],
                    1.0,
                    (0.0, 0.0, 0.0, 0.5).into(),
                )?;

                graphics::draw(ctx, &mesh, graphics::DrawParam::default())?;
            }
        }

        // Draw selections
        if let Some((p1, p2)) = self.selecting {
            let (point, size) = to_rect(p1, p2);

            let rect = graphics::Rect::new(point.x, point.y, size.x, size.y);
            for (color, fill) in &[
                ((0.0, 0.0, 0.0, 0.2).into(), graphics::DrawMode::fill()),
                (graphics::BLACK, graphics::DrawMode::stroke(1.0)),
            ] {
                let mesh = graphics::Mesh::new_rectangle(ctx, *fill, rect, graphics::BLACK)?;
                graphics::draw(ctx, &mesh, graphics::DrawParam::default().color(*color))?;
            }

            for unit in self
                .units
                .iter()
                .filter(|unit| unit.selected_by(point, size))
            {
                draw_circle(ctx, unit.position, 20.0, graphics::BLACK, false)?;
            }
        }

        graphics::present(ctx)?;

        Ok(())
    }

    fn mouse_button_down_event(&mut self, _ctx: &mut Context, button: MouseButton, x: f32, y: f32) {
        if button == MouseButton::Left && x > 0.0 && y > 0.0 {
            self.selecting = Some((Point2::new(x, y), Point2::new(x, y)));
        }
    }

    fn mouse_button_up_event(&mut self, _ctx: &mut Context, button: MouseButton, x: f32, y: f32) {
        if button == MouseButton::Left {
            if let Some((p1, p2)) = self.selecting.take() {
                let (point, size) = to_rect(p1, p2);
                for unit in self.units.iter_mut() {
                    unit.selected = unit.selected_by(point, size);
                }
            }
        } else if button == MouseButton::Right {
            let target = Point2::new(x, y);

            let path = Rc::new(self.game_map.path_flow_to_target(target));

            for unit in self.units.iter_mut() {
                if unit.selected {
                    unit.target = Some(target);
                    unit.path = Some(path.clone());
                }
            }
        }
    }

    fn mouse_motion_event(&mut self, _ctx: &mut Context, x: f32, y: f32, _dx: f32, _dy: f32) {
        if let Some((p1, _)) = self.selecting {
            self.selecting = Some((p1, Point2::new(x, y)));
        }
    }

    fn focus_event(&mut self, _ctx: &mut Context, gained: bool) {
        self.focused = gained;
        self.selecting = None;
    }

    fn resize_event(&mut self, _ctx: &mut Context, width: f32, height: f32) {
        self.window_size = Vector2::new(width, height);
    }
}

fn main() -> GameResult {
    let (mut ctx, mut event_loop) = ContextBuilder::new("MyId", "MyAuthor")
        .window_setup(ggez::conf::WindowSetup {
            title: "Tile RTS".into(),
            ..Default::default()
        })
        .window_mode(ggez::conf::WindowMode {
            width: 1024.0,
            height: 768.0,
            resizable: false,
            ..Default::default()
        })
        .add_resource_path(std::path::PathBuf::from("./resources"))
        .build()?;

    let mut my_game = MyGame::new(&mut ctx)?;

    my_game.units.push(Unit {
        position: Point2::new(200.0, 200.0),
        rotation: Vector2::new(0.0, -1.0),
        intended_direction: None,
        selected: false,
        target: None,
        path: None,
    });

    my_game.units.push(Unit {
        position: Point2::new(300.0, 600.0),
        rotation: Vector2::new(0.0, -1.0),
        intended_direction: None,
        selected: false,
        target: None,
        path: None,
    });

    event::run(&mut ctx, &mut event_loop, &mut my_game)?;

    Ok(())
}
