package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	f, err := os.Open("./input.txt")
	if err != nil {
		panic(err)
	}
	l1, l2, err := fileParser(f)
	if err != nil {
		panic(err)
	}

	diff, err := getDistances(l1, l2)
	if err != nil {
		panic(err)
	}

	fmt.Printf("Value is %d\n", diff)
	sim := similarity(l1, l2)
	fmt.Printf("Similarity is %d\n", sim)
}

func lineParser(line string) (int, int, error) {
	nums := strings.Fields(line)
	if len(nums) != 2 {
		return 0, 0, errors.New("error")
	}
	num1, err := strconv.Atoi(nums[0])
	if err != nil {
		return 0, 0, err
	}
	num2, err := strconv.Atoi(nums[1])
	if err != nil {
		return 0, 0, err
	}
	return num1, num2, nil
}

func fileParser(f io.Reader) ([]int, []int, error) {
	scanner := bufio.NewScanner(f)
	list1, list2 := []int{}, []int{}
	for scanner.Scan() {
		line := scanner.Text()
		num1, num2, err := lineParser(line)
		if err != nil {
			return nil, nil, err
		}
		list1 = append(list1, num1)
		list2 = append(list2, num2)
	}
	return list1, list2, nil
}

func getDistances(l1, l2 []int) (int, error) {
	if len(l1) != len(l2) {
		return 0, errors.New("different lengths")
	}
	slices.Sort(l1)
	slices.Sort(l2)
	diff := 0
	for i := 0; i < len(l1); i++ {

		diff += abs(l1[i] - l2[i])
	}
	return diff, nil
}

type Number interface {
	int | float64
}

func abs[N Number](a N) N {
	if a < 0 {
		a = -a
	}
	return a
}

func similarity(l1, l2 []int) int {
	l2cache := make(map[int]int)
	for _, n := range l2 {
		l2cache[n] += 1
	}
	sim := 0
	for _, n := range l1 {
		sim += n * l2cache[n]
	}

	return sim
}
