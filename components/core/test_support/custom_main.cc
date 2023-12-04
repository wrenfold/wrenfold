#include <gtest/gtest.h>

// https://stackoverflow.com/questions/45575863/how-to-print-utf-8-strings-to-stdcout-on-windows
#if defined(_WIN32) || defined(WIN32)
#include <Windows.h>
class string_buf : public std::stringbuf {
 public:
  int sync() override {
    // Copy string contents to stdout, and reset the buffer:
    fputs(str().c_str(), stdout);
    str("");
    return 0;
  }
};
#endif

// Custom main for use on windows.
int main(int argc, char** argv) {
#if defined(_WIN32) || defined(WIN32)
  // Enable UTF-8 codepage.
  SetConsoleOutputCP(CP_UTF8);
  // Disable buffering of stdout:
  setvbuf(stdout, nullptr, _IONBF, 0);
  // Replace stdout buffer w/ string_buf:
  string_buf buf;
  std::cout.rdbuf(&buf);
#endif
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
