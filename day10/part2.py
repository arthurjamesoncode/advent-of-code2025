import time
import re
import scipy

with open("input.txt", 'r') as f:
    inp = f.read()

inp = inp.split('\n')
if len(inp[-1]) == 0:
    inp.pop()

machines = []
buttons = []
joltages = []

for line in inp:
    mach = re.findall(r'\[([^\]]*)\]', line)
    machines.append(mach[0])

    buts = re.findall(r'\(([^\(]*)\)', line)
    buts = [[int(x) for x in but.split(',')] for but in buts]
    buttons.append(buts)

    jolts = re.findall(r'\{([^\{]*)\}', line)
    jolts = [int(x) for x in jolts[0].split(',')]
    joltages.append(jolts)


def part2():
    ans = 0
    for i, jolts in enumerate(joltages):
        buts = buttons[i]
        
        A = [[0 for i_ in range(len(buts))] for j in range(len(jolts))]
        for j, but in enumerate(buts):
            for light in but:
                A[light][j] = 1

        c = [1 for i_ in range(len(buts))]
        res = scipy.optimize.linprog(c, A_eq=A, b_eq=jolts, integrality=1)

        if not res.success:
            print("Couldn't find optimal solution")
            return -1

        ans += sum(res.x)
    return ans

start = time.time()

# print(part1())
print(part2())

end = time.time()
print(f"Took {(end - start) * 1000} ms")