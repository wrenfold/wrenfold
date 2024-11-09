// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.

#ifndef WF_C_SPAN_TYPES_H
#define WF_C_SPAN_TYPES_H

#ifdef __cplusplus
extern "C" {
#endif  // __cplusplus

#include <assert.h>
#include <stdint.h>  //  uint32_t

// A const 2D span over double precision values.
struct input_span2d {
  const double* data;
  uint32_t rows;
  uint32_t cols;
  uint32_t stride;  //  Stride for column-major ordered data.
};
typedef struct input_span2d input_span2d_t;

// A non-const 2D span over double precision values.
struct output_span2d {
  double* data;
  uint32_t rows;
  uint32_t cols;
  uint32_t stride;
};
typedef struct output_span2d output_span2d_t;

// Read a value from an instance of `input_span2d`.
// We assume a column-major ordering to match Eigen.
inline double get_input_value(const input_span2d_t span, const uint32_t row, const uint32_t col) {
  assert(span.data);
  assert(row < span->rows);
  assert(col < span->cols);
  return span.data[row + col * span.stride];
}

// Write a value to an instance of `output_span2d`.
inline void set_output_value(const output_span2d_t* span, const uint32_t row, const uint32_t col,
                             const double value) {
  assert(span->data);
  assert(row < span->rows);
  assert(col < span->cols);
  span->data[row + col * span->stride] = value;
}

// A simple quaternion struct.
struct quaternion {
  double w;
  double x;
  double y;
  double z;
};
typedef struct quaternion quaternion_t;

#ifdef __cplusplus
}
#endif  // __cplusplus

#endif  // WF_C_SPAN_TYPES_H
