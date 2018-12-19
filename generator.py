from random import randint
import sys

if __name__ == "__main__":
  n = 50
  try:
    n = sys.argv[1]
  except:
    pass
  with open("input.txt", "w+") as f:
    for i in range(1,n):
      f.write("p{0} {1} {2} {3}\n".format(i, i*3, randint(1, 1000), randint(5, 25)))

    