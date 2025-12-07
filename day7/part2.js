const { count } = require('console')
const fs = require('fs')

function parse (input) {
  return input.split('\n')
}

function findStart (grid) {
  for (let i = 0; i < grid[0].length; i++) {
    if(grid[0][i] === 'S') return i
  }

  return -1
}

function cached_count (grid, start_pos) {
  const cache = {}

  function count (grid, pos) {
    const [row, col] = pos

    if (cache[`${row},${col}`] !== undefined) {
      return cache[`${row},${col}`]
    }

    if (row >= grid.length) {
      return 0
    }

    if (grid[row][col] !== '^') {
      const total = count (grid, [row+1, col])
      cache[`${row},${col}`] = total
      return total
    }

    const left_count  = count (grid, [row, col-1])
    const right_count = count (grid, [row, col+1])

    const total = left_count + right_count + 1
    cache[`${row},${col}`] = total 
    // console.log("left", left_count, "right", right_count, total)
    return total
  }

  return count(grid, start_pos) + 1
}

function main (input) {
  const grid = parse(input)
  const start = findStart(grid)

  return cached_count (grid, [0, start])
}

test  = fs.readFileSync("./test.txt", "utf8")
input = fs.readFileSync("./input.txt", "utf8")

console.log("Test", main (test))
console.log("Input", main (input))