#include <iostream>
#include <thread>
#include <string>
#include <atomic>
#include <condition_variable>
#include <list>
#include <chrono>


#define NUMTOPTHREADS 2

[[noreturn]] static void threadPrintWorld();
[[noreturn]] static void threadPrintHello();

static std::list<std::string> printthreadPrintHello;
static std::list<std::string> printthreadPrintWorld;
static std::atomic<int> rendezvousCounter;
static std::mutex rendezvousSyncMutex;
static std::condition_variable cv;
static void waitForRendezvous(std::string name)
{
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex);
  ++rendezvousCounter;
  if(rendezvousCounter.load() < NUMTOPTHREADS)
  {
    cv.wait(lk);
  }
  else if (rendezvousCounter.load() == NUMTOPTHREADS)
  {
    while(!printthreadPrintHello.empty()){
std::cout << printthreadPrintHello.front();
printthreadPrintHello.pop_front();
}
while(!printthreadPrintWorld.empty()){
std::cout << printthreadPrintWorld.front();
printthreadPrintWorld.pop_front();
}

    {
      rendezvousCounter.store(0);
      cv.notify_all();
    }
  }
  else
  {
    std::cout << "error in wait for " << name << ". Rendezvouscounter out of bounds. RedezvousCounter = " << rendezvousCounter.load() << "\n";
    exit(0);
  }
}



[[noreturn]] static void threadPrintWorld()
{while(true)
{
  printthreadPrintWorld.push_back("World\n");
  waitForRendezvous("threadPrintWorld");
  continue;
}
}

[[noreturn]] static void threadPrintHello()
{while(true)
{
  printthreadPrintHello.push_back("Hello ");
  waitForRendezvous("threadPrintHello");
  continue;
}
}


int main()
{
rendezvousCounter.store(0);

std::thread tthreadPrintHello (threadPrintHello);
std::thread tthreadPrintWorld (threadPrintWorld);
while(true)
{
std::this_thread::sleep_for(std::chrono::seconds(1));
}
}