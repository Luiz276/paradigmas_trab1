import math


def check_sub_grid(grid, column, row, num):
    cells = math.floor(math.sqrt(len(grid)))
    grid_row = (row // cells) * cells
    grid_column = (column // cells) * cells


    for i in range(grid_row, grid_row + cells):
        for j in range(grid_column, grid_column + cells):
            if grid[i][j] == num:
                return False

    return True

def check_column(grid, column, row, num):
    for i in range(len(grid)):
        if grid[i][column] == num:
            return False
    return True

def check_row(grid, column, row, num):
    for i in range(len(grid)):
        if grid[row][i] == num:
            return False
    return True

# retorna uma posição que ainda nao foi preenchida
def search_available(grid):
    for i in range(len(grid)):
        for j in range(len(grid)):
            if grid[i][j] == 0:
                return (i, j)
    return (-1, -1)

def solve_sudoku(grid):
    row, column = search_available(grid)

    if column == -1:
        return True

    for num in range(1, 10):
        
        if(check_row(grid, column, row, num) and check_column(grid, column, row, num) and check_sub_grid(grid, column, row, num)):
            grid[row][column] = num

            if solve_sudoku(grid):
                return True

            grid[row][column] = 0

    return False


    

if __name__ == "__main__":
    # grid =[[3, 0, 6, 5, 0, 8, 4, 0, 0],
    #         [5, 2, 0, 0, 0, 0, 0, 0, 0],
    #         [0, 8, 7, 0, 0, 0, 0, 3, 1],
    #         [0, 0, 3, 0, 1, 0, 0, 8, 0],
    #         [9, 0, 0, 8, 6, 3, 0, 0, 5],
    #         [0, 5, 0, 0, 9, 0, 6, 0, 0],
    #         [1, 3, 0, 0, 0, 0, 2, 5, 0],
    #         [0, 0, 0, 0, 0, 0, 0, 7, 4],
    #         [0, 0, 5, 2, 0, 6, 3, 0, 0]]
    grid = [[0 for j in range(9)] for i in range(9)]

    result = grid if solve_sudoku(grid) else "no possible solution"
    print(result)

    