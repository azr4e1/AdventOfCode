package day12_test

import (
	"day12"
	"github.com/google/go-cmp/cmp"
	"testing"
)

func TestParseReturnsTheCorrectInputStruct(t *testing.T) {
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
