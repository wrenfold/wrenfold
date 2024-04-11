// Copyright 2024 Gareth Cross
#pragma once
#include <chrono>
#include <memory>
#include <string_view>

#ifdef WF_ENABLE_TRACING
// Concatenate `a` with `b`.
#define WF_CONCAT_(a, b) a##b
#define WF_CONCAT(a, b) WF_CONCAT_(a, b)

// Create a scoped trace with the provided name.
// This version of the macro accepts a string literal.
#define WF_SCOPED_TRACE_STR(str) \
  wf::scoped_trace WF_CONCAT(__timer, __LINE__) { str }
#else
// Do nothing when tracing is disabled.
#define WF_SCOPED_TRACE_STR(str)
#endif  // WF_ENABLE_TRACING

#define WF_SCOPED_TRACE(name) WF_SCOPED_TRACE_STR(#name)
#define WF_FUNCTION_TRACE() WF_SCOPED_TRACE_STR(__FUNCTION__)

namespace wf {

// Fields are documented in this doc - I only support the bare minimum required:
// https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/
struct trace_event {
  // Name of the event.
  std::string_view name;
  // Timestamp in microseconds.
  std::int64_t ts;
  // Process ID.
  std::uint32_t pid;
  // Thread ID.
  std::uint32_t tid;
  // Duration in nanoseconds.
  std::int64_t dur_ns;
};

// Aggregates tracing events and writes them out in chrome://trace format.
class trace_collector {
 public:
  trace_collector();

  // Write all traces out on destruction, assuming no exceptions are in flight.
  ~trace_collector();

  // Access global instance of the trace collector.
  static trace_collector* get_instance();

  // Log an event.
  void submit_event(trace_event event);

  // Set the output path for traces.
  void set_output_path(const std::string& path);

 private:
  struct trace_collector_impl& impl() noexcept;

  // Write all traces to disk as JSON.
  void write_traces() const;

  // Minimize header bloat with pimpl pattern.
  std::unique_ptr<trace_collector_impl> impl_;
};

// Measure time elapsed in a particular scope.
class scoped_trace {
 public:
  explicit scoped_trace(const std::string_view name) noexcept
      : name_(name), start_(std::chrono::high_resolution_clock::now()) {}
  ~scoped_trace();

  // non-copyable and non-moveable
  scoped_trace(const scoped_trace&) = delete;
  scoped_trace(scoped_trace&&) = delete;
  scoped_trace& operator=(const scoped_trace&) = delete;
  scoped_trace& operator=(scoped_trace&&) = delete;

 private:
  std::string_view name_;
  std::chrono::high_resolution_clock::time_point start_;
};

}  // namespace wf
