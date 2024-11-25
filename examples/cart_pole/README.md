# cart_pole

## Running the python test

```bash
python -m examples.cart_pole.cart_pole_test
```

The python test generates the dynamics in JAX+NumPy. It serves as an example of generating symbolic function and invoking it in Python.

## Running the demo

This example includes an [egui](https://github.com/emilk/egui)-based visualization of the resulting cart-pole system. You can view the visualizations using:

```bash
cargo run -p cart_pole_demo --release
```

For a more full-featured example (also made with wrenfold), see the [cart-pole-mpc](https://github.com/gareth-cross/cart-pole-mpc) demo application.
