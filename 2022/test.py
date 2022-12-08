import numpy as np

grid = np.array([list(x.strip()) for x in open("08/input")], int)
part1 = np.zeros_like(grid, int)
part2 = np.ones_like(grid, int)

for _ in range(4):
    for x, y in np.ndindex(grid.shape):
        lower = [t < grid[x, y] for t in grid[x, y + 1 :]]

        part1[x, y] |= all(lower)
        part2[x, y] *= next((i + 1 for i, t in enumerate(lower) if ~t), len(lower))

    grid, part1, part2 = map(np.rot90, [grid, part1, part2])

print(part2)
print(part1.sum(), part2.max())
