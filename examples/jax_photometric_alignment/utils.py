import gzip
import typing as T
from pathlib import Path

import jax
import jax.numpy as jnp
import numpy as np
from numpy.typing import DTypeLike, NDArray


def load_gzipped_image(name: str, dtype: DTypeLike, shape: T.Tuple[int, int]) -> NDArray:
    current_dir = Path(__file__).parent.absolute()
    image_path = current_dir / "images" / name
    with gzip.open(image_path, "rb") as handle:
        data = handle.read()
    return np.frombuffer(data, dtype=dtype).reshape(shape)


def build_image_pyramid(image: NDArray,
                        num_levels: int = 3,
                        output_dtype: DTypeLike = jnp.uint8) -> T.List[jnp.ndarray]:
    """
    Create a gaussian image pyramid using JAX methods.

    We could use OpenCV here, but I'd like to avoid another test-time dependency.
    """
    gaussian_kernel = (
        np.array(
            [
                [1, 4, 6, 4, 1],
                [4, 16, 24, 16, 4],
                [6, 24, 36, 24, 6],
                [4, 16, 24, 16, 4],
                [1, 4, 6, 4, 1],
            ],
            dtype=np.float32,
        ) / 256.0)

    pyramid = [jnp.asarray(image)]
    previous = image.astype(np.float32)
    for _ in range(0, num_levels - 1):
        blurred = jax.scipy.signal.convolve2d(previous, gaussian_kernel, mode="same")
        downsized = jax.image.resize(
            image=blurred,
            shape=[previous.shape[0] // 2, previous.shape[1] // 2],
            method=jax.image.ResizeMethod.LINEAR,
        )
        previous = downsized
        pyramid.append(downsized.astype(output_dtype))

    return pyramid


def create_src_pts_array(shape: T.Tuple[int, int]) -> jnp.ndarray:
    """
    Create an array of pixel coordinates with the provided shape (specified as (rows, cols)).

    Returns a [rows*cols, 2] shaped array in row-major order starting from (0, 0).
    """
    y, x = np.mgrid[0:shape[0], 0:shape[1]]
    xy = np.dstack([x, y]).reshape([-1, 2])
    return jnp.asarray(xy, dtype=jnp.float32)
