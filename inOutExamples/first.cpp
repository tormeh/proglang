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



///////////////////////compile with:    clang++ -std=c++11 -Weverything first.cpp && ./a.out && rm a.out 


#include <iostream>
#include <thread>
#include <string>
#include <atomic>
#include <condition_variable>
#include <list>

/*std::mutex rendezvousMutex;
int rendezvousCounter;

void incrementRendezvousCounter()
{
  rendezvousMutex.lock()
  rendezvousCounter++;
  rendezvousMutex.unlock()
}*/

static std::atomic<int> rendezvousCounter;
static std::mutex rendezvousSyncMutex;
static std::condition_variable cv;

static void threadPrintHello(std::list<std::string> println, int syncd)
{
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex);
  println.push_back("Hello");
  ++rendezvousCounter;
  cv.wait(lk);
  threadPrintHello(println, syncd);
}

static void threadPrintWorld(std::list<std::string> println, int syncd)
{
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex);
  println.push_back(" world\n");
  ++rendezvousCounter;
  threadPrintWorld(println, syncd);
}

int main()
{
  //std::cout << "Hello World!\n";
  rendezvousCounter.store(0);
  int synchronizedNumber = 0;
  std::list<std::string> out1;
  std::list<std::string> out2;
  std::thread t1 (threadPrintHello, out1, synchronizedNumber);
  std::thread t2 (threadPrintWorld, out2, synchronizedNumber);
  while(true)
  {
    if(rendezvousCounter.load() == 2)
    {
      while(!out1.empty())
      {
        std::cout << out1.front();
        out1.pop_front();
      }
      while(!out2.empty())
      {
        std::cout << out2.front();
        out2.pop_front();
      }
      
      rendezvousCounter.store(0);
      cv.notify_all();
    }
    
  }
  //t1.join();
  //t2.join();
}
