import time
start = time.time()

exec(open('./mnist_cnn.py').read())

end = time.time()
print(end - start)
