package day12_test

import (
	"day12"
	"github.com/google/go-cmp/cmp"
	"testing"
)

type TestCase struct {
	Test   day12.Input
	Answer int
}

func TestParseReturnsTheCorrectInputStruct(t *testing.T) {
	t.Parallel()
	input := "???.### 1,1,3"
	want := day12.Input{
		Row:    "???.###",
		Groups: []int{1, 1, 3},
	}
	got := day12.Parse(input)
	if !cmp.Equal(got, want) {
		t.Error(cmp.Diff(want, got))
	}
}

func TestCountConfigurations_ReturnsTheCorrectNumberOfValidConfigurations(t *testing.T) {
	t.Parallel()
	testCases := []TestCase{
		{Test: day12.Input{"???.###", []int{1, 1, 3}}, Answer: 1},
		{Test: day12.Input{".??..??...?##.", []int{1, 1, 3}}, Answer: 4},
		{Test: day12.Input{"?#?#?#?#?#?#?#?", []int{1, 3, 1, 6}}, Answer: 1},
		{Test: day12.Input{"????.#...#...", []int{4, 1, 1}}, Answer: 1},
		{Test: day12.Input{"????.######..#####.", []int{1, 6, 5}}, Answer: 4},
		{Test: day12.Input{"?###????????", []int{3, 2, 1}}, Answer: 10},
	}

	for _, tc := range testCases {
		want := tc.Answer
		got := day12.CountConfigurations(tc.Test)

		if want != got {
			t.Errorf("want %d, got %d for test %q", want, got, tc.Test)
		}
	}
}
