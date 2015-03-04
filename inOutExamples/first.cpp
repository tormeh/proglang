/*

program p:Nothing =
{
  synchronized variable synchronizedNumber:Integer = {0}
  threadPrintHello(synchronizedNumber)
  threadPrintWorld(synchronizedNumber)
}
  
thread threadPrintHello(in:Integer):Nothing =
{
  println("hello ")
  threadPrintHello()
}

thread threadPrintWorld(in:Integer):Nothing =
{
  println("world")
  threadPrintWorld()
}

*/





#include <iostream>
#include <thread>
#include <string>
#include <atomic>
#include <condition_variable>

/*std::mutex rendezvousMutex;
int rendezvousCounter;

void incrementRendezvousCounter()
{
  rendezvousMutex.lock()
  rendezvousCounter++;
  rendezvousMutex.unlock()
}*/

std::atomic<int> rendezvousCounter;
std::mutex rendezvousSyncMutex;
std::condition_variable cv;

void threadPrintHello(std::list<string> println, syncd)
{
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex)
  println.push_back("Hello");
  ++rendezvousCounter;
  cv.wait(lk)
  threadPrintHello();
}

void threadPrintWorld(std::list<string> println, syncd)
{
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex)
  println.push_back(" world\n");
  ++rendezvousCounter;
  threadPrintWorld();
}

int main()
{
  //std::cout << "Hello World!\n";
  rendezvousCounter.store(0);
  std::list<string> out1;
  std::list<string> out2;
  std::thread t1(threadPrintHello(out1, synchronizedNumber));
  std::thread t2(threadPrintWorld(out2, synchronizedNumber));
  while(true)
  {
    if(rendezvousCounter.load() == 2)
    {
      for(int i=0; i<out1.length, i++)
      {
        std::cout << out1[i]
      }
      for(int i=0; i<out2.length, i++)
      {
        std::cout << out2[i]
      }
      rendezvousCounter.store(0);
      cv.notify_all;
    }
    
  }
  t1.join();
  t2.join();
}
