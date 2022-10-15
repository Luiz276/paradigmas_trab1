import math
import random

def create_comparison(size):
    comp_hor = []
    comp_ver = []
    cells = math.floor(math.sqrt(size))
    for i in range(size):
        comp_line = []
        for j in range(1, size):

            comparator = random.choice([">", "<"])
            if j % cells == 0:
                comparator = 'X'

            comp_line.append(comparator)
        comp_hor.append(comp_line)

    for i in range(1, size):
        comp_line = []
        for j in range(size):

            comparator = random.choice([">", "<"])
            if i % cells == 0:
                comparator = 'X'

            comp_line.append(comparator)
        comp_ver.append(comp_line)

    return (comp_hor, comp_ver)
    
    
def get_neighboors(grid, column, row):
    cells = math.floor(math.sqrt(len(grid)))
    relative_row = row % cells
    relative_column = column % cells

    neighboors = []
    if relative_row - 1 >= 0:
        neighboors.append((row-1, column, grid[row-1][column]))
    # if row + 1 < cells:
    #     neighboors.append((row+1, column, grid[row+1][column]))
    if relative_column - 1 >= 0:
        neighboors.append((row, column-1, grid[row][column-1]))
    # if column + 1 < cells:
    #     neighboors.append((row, column+1, grid[row][column+1]))

    return neighboors

def string_comparison(left_num, comp_char, right_num):
    if comp_char == '>':
        return left_num > right_num
    else:
        return left_num < right_num

def check_comparisons(grid, column, row, comp_hor, comp_vert, num):
    neighboors = get_neighboors(grid, column, row)
    for neighboor in neighboors:
        neighboor_row = neighboor[0]
        neighboor_column = neighboor[1]
        neighboor_value = neighboor[2]
        if neighboor_value == 0:
            continue
        i = min(row, neighboor_row)
        j = min(column, neighboor_column)
        comparisson = comp_hor[i][j] if row == neighboor_row else comp_vert[i][j]
        if not (string_comparison(neighboor_value, comparisson, num)):
            return False

    return True

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

def solve_sudoku(grid, comp_hor, comp_ver):
    row, column = search_available(grid)

    if column == -1:
        return True

    for num in range(1, 10):
        
        if(check_row(grid, column, row, num) and check_column(grid, column, row, num) and check_sub_grid(grid, column, row, num) and check_comparisons(grid, column, row, comp_hor, comp_ver, num)):
            grid[row][column] = num

            if solve_sudoku(grid, comp_hor, comp_ver):
                return True

            grid[row][column] = 0
            # print("------------------")
            # print_grid(grid)

    return False

def print_grid(grid):
    for i in grid:
        print(i)
    

if __name__ == "__main__":
    grid = [[0 for j in range(9)] for i in range(9)]

    (comp_hor, comp_ver) = create_comparison(9)
    comp_hor = [[">","<","X",">","<","X","<",">"],
                [">","<","X",">",">","X","<",">"],
                ["<","<","X",">",">","X",">","<"],
                [">",">","X","<",">","X","<","<"],
                ["<",">","X",">",">","X",">","<"],
                ["<",">","X","<","<","X","<",">"],
                [">","<","X",">","<","X",">","<"],
                ["<",">","X",">","<","X","<",">"],
                ["<",">","X","<",">","X",">",">"]]
    
    comp_ver = [["<","<","<",">","<",">","<","<",">"],
                [">",">","<",">",">","<","<",">","<"],
                ["X","X","X","X","X","X","X","X","X"],
                [">","<","<","<",">",">","<",">",">"],
                [">",">",">",">","<","<",">","<",">"],
                ["X","X","X","X","X","X","X","X","X"],
                ["<","<",">",">",">",">","<","<",">"],
                [">",">","<",">","<","<","<",">",">"]]

    result = grid if solve_sudoku(grid, comp_hor, comp_ver) else "no possible solution"
    print(result)
    print_grid(grid)
    

    