package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

const MAXDIFF = 3

type Report struct {
	levels []int
}

func ReadReports(file io.Reader) ([]Report, error) {
	content, err := io.ReadAll(file)
	if err != nil {
		return nil, err
	}
	repS := strings.Split(string(content), "\n")
	reports := []Report{}
	for _, r := range repS {
		levelsS := strings.Fields(r)
		levels := make([]int, len(levelsS))
		for i, v := range levelsS {
			levels[i], err = strconv.Atoi(v)
			if err != nil {
				return nil, err
			}
		}
		reports = append(reports, Report{levels})
	}
	return reports, nil
}

func (r Report) IsSafe() bool {
	isSafe := true
	if len(r.levels) <= 1 {
		return false
	}
	val := r.levels[0]
	prevDiff := []int{0}
	// dampener := 0
	for i := 1; i < len(r.levels); i++ {
		v := r.levels[i]
		diff := val - v
		if !checkDiffCondition(diff) {
			return false
		}
		if !checkMonoCondition(diff, prevDiff[len(prevDiff)-1]) {
			return false
		}
		prevDiff = append(prevDiff, diff)
		val = v
	}
	return isSafe
}

func checkDiffCondition(diff int) bool {
	if diff == 0 || abs(diff) > MAXDIFF {
		return false
	}
	return true
}

func checkMonoCondition(diff, prevDiff int) bool {
	if prevDiff != 0 && prevDiff*diff < 0 {
		return false
	}
	return true
}

// func dampenerEffect(levels, prevDiff []int, i, dampener int) (bool, int) {
// 	if dampener > 0 {
// 		return false, dampener
// 	}
// 	i_prev = i - 2
// 	i_pos
// 	return false, dampener + 1
// }

func abs(a int) int {
	if a <= 0 {
		return -a
	}
	return a
}

func main() {
	f, err := os.Open("./input.txt")
	if err != nil {
		panic(err)
	}
	reports, err := ReadReports(f)
	if err != nil {
		panic(err)
	}
	trueCounts := 0
	for _, r := range reports {
		if r.IsSafe() {
			trueCounts++
		}
	}

	fmt.Printf("Number of safe reports: %d", trueCounts)
}
