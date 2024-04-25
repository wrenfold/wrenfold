// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/scoped_trace.h"

#include <deque>
#include <filesystem>
#include <fstream>
#include <mutex>
#include <optional>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else
#include <unistd.h>  // getpid
#ifdef __APPLE__
#include <pthread.h>  // pthread_mach_thread_np
#else
#include <sys/syscall.h>  // syscall
#include <sys/types.h>
#endif  // __APPLE__
#endif  // _WIN32

#include "wf/assertions.h"
#include "wf/third_party_imports.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

// Format wf::trace_event to JSON.
template <>
struct fmt::formatter<wf::trace_event> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::trace_event& event, FormatContext& ctx) const -> decltype(ctx.out()) {
    constexpr std::string_view phase = "X";  //  Complete event.
    constexpr std::string_view fmt_template =
        R"json({{ "name": "{name}", "cat": "", "ph": "{ph}", "ts": {ts}, "pid": {pid}, "tid": {tid}, "dur": {dur}, "dur_ns": {dur_ns} }})json";
    return fmt::format_to(ctx.out(), fmt_template, fmt::arg("name", event.name),
                          fmt::arg("ph", phase), fmt::arg("ts", event.ts),
                          fmt::arg("pid", event.pid), fmt::arg("tid", event.tid),
                          fmt::arg("dur", event.dur_ns / 1000), fmt::arg("dur_ns", event.dur_ns));
  }
};

namespace wf {

// Object that initializes the PID and TID on construction.
struct pid_and_tid {
  pid_and_tid() noexcept {
#ifdef _WIN32
    // These return long (on 64-bit windows), but the underlying value is 32 bits.
    pid = static_cast<std::uint32_t>(GetCurrentProcessId());
    tid = static_cast<std::uint32_t>(GetCurrentThreadId());
#elif defined(__APPLE__)
    pid = static_cast<std::uint32_t>(getpid());
    tid = static_cast<std::uint32_t>(pthread_mach_thread_np(pthread_self()));
#else
    // Linux
    // https://stackoverflow.com/questions/21091000
    pid = static_cast<std::uint32_t>(getpid());
    tid = static_cast<std::uint32_t>(syscall(__NR_gettid));
#endif
  }

  std::uint32_t pid;
  std::uint32_t tid;
};

struct trace_collector_impl {
  std::deque<trace_event> events;
  std::mutex mutex;
  std::string output_path{};
};

trace_collector* trace_collector::get_instance() {
  static trace_collector global_collector{};
  return &global_collector;
}

inline constexpr std::string_view json_object_template = R"json(
{{
  "traceEvents": [
    {}
  ],
  "displayTimeUnit": "ns"
}}
)json";

trace_collector::trace_collector() : impl_(std::make_unique<trace_collector_impl>()) {
  WF_ASSERT(impl_);
}

trace_collector::~trace_collector() {
  if (std::uncaught_exceptions() > 0) {
    // Don't do anything that could throw if exceptions are in flight.
    return;
  }
  write_traces();
}

void trace_collector::submit_event(trace_event event) {
  // TODO: Do this without locking+unlocking.
  std::lock_guard guard{impl().mutex};
  impl().events.push_back(std::move(event));
}

void trace_collector::set_output_path(const std::string& path) { impl().output_path = path; }

// ReSharper disable once CppMemberFunctionMayBeConst
trace_collector_impl& trace_collector::impl() noexcept { return *impl_; }

void trace_collector::write_traces() const {
  // Format to JSON and write to the output path.
  if (!impl_->output_path.empty()) {
    const auto abs_path = std::filesystem::weakly_canonical(impl_->output_path);
    if (std::ofstream output(abs_path); output.good()) {
      fmt::print("Writing trace events to: {}\n", abs_path.string());
      output << fmt::format(json_object_template, fmt::join(impl_->events, ",\n    "));
      output.flush();
    }
  }
}

template <typename U, typename T>
static auto cast_time(const T& dur) {
  return std::chrono::duration_cast<U>(dur).count();
}

scoped_trace::~scoped_trace() {
  const auto end = std::chrono::high_resolution_clock::now();
  if (std::uncaught_exceptions() > 0) {
    return;
  }
  if (trace_collector* const collector = trace_collector::get_instance(); collector != nullptr) {
    // Our events are "complete" events, so we compute the `dur` field.
    const std::int64_t duration_nanos = cast_time<std::chrono::nanoseconds>(end - start_);
    const std::int64_t start_micros =
        cast_time<std::chrono::microseconds>(start_.time_since_epoch());

    const thread_local pid_and_tid ids{};
    collector->submit_event(trace_event{name_, start_micros, ids.pid, ids.tid, duration_nanos});
  }
}

}  // namespace wf
