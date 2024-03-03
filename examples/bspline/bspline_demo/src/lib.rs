use std::collections::HashMap;

#[cfg(target_arch = "wasm32")]
use eframe::wasm_bindgen::{self, prelude::*};

use egui::Color32;
use egui_plot::{Line, Plot, PlotPoints};
use nalgebra as na;

use bspline_rust_test as bsplines;

/// Main struct of the web-app.
pub struct WebApp {
    /// Number of knots to render in our demo spline.
    num_knots: usize,
}

impl WebApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        egui_extras::install_image_loaders(&cc.egui_ctx);
        Self { num_knots: 11 }
    }
}

impl eframe::App for WebApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("top_bar").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.label("Spline Demo");
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            let order = 7;

            ui.vertical(|ui| {
                ui.add(egui::Slider::new(&mut self.num_knots, (order + 1)..=32));
                // if ui.button("Click me!").clicked() {
                //     // panic!("OH FUCK!");
                // }
                ui.separator();

                let mut plot = Plot::new("plot").show_axes(true).show_grid(true);

                plot = plot
                    .view_aspect(1.0)
                    .width(1280.0)
                    .height(720.0)
                    .allow_scroll(false)
                    .allow_boxed_zoom(false)
                    .legend(egui_plot::Legend::default());

                plot.show(ui, |plot_ui| {
                    let num_pts = 1000_usize;
                    // let num_knots = 11;

                    let num_intervals = self.num_knots - 1;
                    let num_basis_functions = num_intervals + order;

                    let mut pts = Vec::new();
                    pts.resize(num_basis_functions, Vec::<[f64; 2]>::with_capacity(num_pts));

                    // // pts.resize(num_knots + 4 - 1, value) // 13
                    // let mut pts = HashMap::<usize>::new();

                    for i in 0..num_pts {
                        let t = (i as f64 + 0.5) / (num_pts as f64);

                        let mut coeffs = na::SMatrix::<f64, 7, 6>::zeros();
                        let index = bsplines::eval_bspline_coefficients_order7(
                            t,
                            self.num_knots,
                            &mut coeffs,
                        );

                        for j in 0..order {
                            pts.get_mut(index + j).unwrap().push([t, coeffs[(j, 0)]]);
                        }
                    }

                    for (index, mut values) in pts.into_iter().enumerate() {
                        values.sort_by(|[t0, _], [t1, _]| t0.total_cmp(t1));

                        let circle_points = PlotPoints::from(values);
                        let line = Line::new(circle_points)
                            .color(Color32::from_rgb(100, 200, 100))
                            .style(egui_plot::LineStyle::Solid)
                            .name(format!("Interval {:02}", index));

                        plot_ui.line(line);
                    }
                });
            });
        });
    }

    #[cfg(target_arch = "wasm32")]
    fn as_any_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        Some(&mut *self)
    }
}

#[cfg(target_arch = "wasm32")]
#[derive(Clone)]
#[wasm_bindgen]
pub struct WebHandle {
    runner: eframe::WebRunner,
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
impl WebHandle {
    /// Installs a panic hook, then returns.
    #[allow(clippy::new_without_default)]
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        // Redirect [`log`] message to `console.log` and friends:
        eframe::WebLogger::init(log::LevelFilter::Debug).ok();

        Self {
            runner: eframe::WebRunner::new(),
        }
    }

    /// Call this once from JavaScript to start your app.
    #[wasm_bindgen]
    pub async fn start(&self, canvas_id: &str) -> Result<(), wasm_bindgen::JsValue> {
        self.runner
            .start(
                canvas_id,
                eframe::WebOptions::default(),
                Box::new(|cc| Box::new(WebApp::new(cc))),
            )
            .await
    }

    #[wasm_bindgen]
    pub fn destroy(&self) {
        self.runner.destroy();
    }

    /// The JavaScript can check whether or not your app has crashed:
    #[wasm_bindgen]
    pub fn has_panicked(&self) -> bool {
        self.runner.has_panicked()
    }

    #[wasm_bindgen]
    pub fn panic_message(&self) -> Option<String> {
        self.runner.panic_summary().map(|s| s.message())
    }

    #[wasm_bindgen]
    pub fn panic_callstack(&self) -> Option<String> {
        self.runner.panic_summary().map(|s| s.callstack())
    }
}
