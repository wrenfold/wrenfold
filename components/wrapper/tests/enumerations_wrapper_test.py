import enum
import unittest

from wrenfold import enumerations


class EnumerationsWrapperTest(unittest.TestCase):
    def test_std_function_to_string(self):
        """Test that we can call to_string() on StdMathFunction."""
        self.assertEqual(enumerations.StdMathFunction.Sinh.to_string(), "sinh")
        self.assertEqual(enumerations.StdMathFunction.Sqrt.to_string(), "sqrt")

    def test_enums_exist(self):
        self.assertIsInstance(enumerations.RelationalOperation.LessThan, enum.Enum)
        self.assertIsInstance(enumerations.SymbolicConstant.Pi, enum.Enum)
        self.assertIsInstance(enumerations.NumberSet.RealPositive, enum.Enum)


if __name__ == "__main__":
    unittest.main()
