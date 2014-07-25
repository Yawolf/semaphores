import threading
from subprocess import call

class my_thread(threading.Thread):
    def __init__ (self,num):
        threading.Thread.__init__ (self)
        self.num = num

    def run(self):
        command = "/home/santiago.cervantes/file_lock/test" + str (self.num)
        call (command)

def main ():
    p = my_thread(1)
    s = my_thread(2)
    p.start()
    s.start()
    p.join()
    s.join()


if __name__ == "__main__":
    main()
