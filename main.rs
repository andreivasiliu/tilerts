use ggez::event::{self, EventHandler, MouseButton};
use ggez::graphics;
use ggez::{Context, ContextBuilder, GameResult};

#[derive(Default)]
struct MyGame {
    focus_color: f32,
    focused: bool,
    selecting: Option<(f32, f32, f32, f32)>,
}

impl EventHandler for MyGame {
    fn update(&mut self, _ctx: &mut Context) -> GameResult<()> {
        if self.focused && self.focus_color < 0.3 {
            self.focus_color += 0.01;
        } else if !self.focused && self.focus_color > 0.2 {
            self.focus_color -= 0.01;
        }

        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult<()> {
        graphics::clear(ctx, (0.2, 0.2, self.focus_color).into());

        if let Some((x1, y1, x2, y2)) = self.selecting {
            let rect =
                graphics::Rect::new(x1.min(x2), y1.min(y2), (x2 - x1).abs(), (y2 - y1).abs());
            for (color, fill) in &[
                ((0.0, 0.0, 0.0, 0.2).into(), graphics::DrawMode::fill()),
                (graphics::BLACK, graphics::DrawMode::stroke(1.0)),
            ] {
                let mesh = graphics::Mesh::new_rectangle(ctx, *fill, rect, graphics::BLACK)?;
                graphics::draw(ctx, &mesh, graphics::DrawParam::default().color(*color))?;
            }
        }

        graphics::present(ctx)?;

        Ok(())
    }

    fn mouse_button_down_event(&mut self, _ctx: &mut Context, button: MouseButton, x: f32, y: f32) {
        if button == MouseButton::Left && x > 0.0 && y > 0.0 {
            self.selecting = Some((x, y, x, y));
        }
    }

    fn mouse_button_up_event(&mut self, _ctx: &mut Context, button: MouseButton, _x: f32, _y: f32) {
        if button == MouseButton::Left {
            self.selecting = None;
        }
    }

    fn mouse_motion_event(&mut self, _ctx: &mut Context, x: f32, y: f32, _dx: f32, _dy: f32) {
        if let Some((x1, y1, _, _)) = self.selecting {
            self.selecting = Some((x1, y1, x, y));
        }
    }

    fn focus_event(&mut self, _ctx: &mut Context, gained: bool) {
        self.focused = gained;
        self.selecting = None;
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
            resizable: true,
            ..Default::default()
        })
        .build()?;

    let mut my_game = MyGame::default();

    event::run(&mut ctx, &mut event_loop, &mut my_game)?;

    Ok(())
}
