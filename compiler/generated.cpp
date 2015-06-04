#include <iostream>
#include <thread>
#include <string>
#include <atomic>
#include <condition_variable>
#include <list>
#include <chrono>


#define NUMTOPTHREADS 1

[[noreturn]] static void threadA();
int factorial$threadA(int i);
void actionPrintSquare$threadA(double d);
double square$threadAactionPrintSquare(double x);

static std::list<std::string> printthreadA;
static std::atomic<int> rendezvousCounter;
static std::mutex rendezvousSyncMutex;
static std::condition_variable cv;
static double threadA$d;
static int threadA$i;
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
    while(!printthreadA.empty()){
std::cout << printthreadA.front();
printthreadA.pop_front();
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



[[noreturn]] static void threadA()
{while(true)
{
  printthreadA.push_back("The factorial of ");
  printthreadA.push_back(std::to_string(threadA$i));
  printthreadA.push_back(" is ");
  printthreadA.push_back(std::to_string(factorial$threadA(threadA$i)));
  printthreadA.push_back("    ");
  actionPrintSquare$threadA(threadA$d);
  waitForRendezvous("threadA");
threadA$d = (threadA$d + 0.5);
threadA$i = (threadA$i + 1);

  continue;
}
}

int factorial$threadA(int i)
{
  return 1 == i ? 1 : (i * factorial$threadA((i - 1))); //returntype: Something
}

void actionPrintSquare$threadA(double d)
{
  printthreadA.push_back("The square of ");
  printthreadA.push_back(std::to_string(d));
  printthreadA.push_back(" is ");
  printthreadA.push_back(std::to_string(square$threadAactionPrintSquare(d)));
  printthreadA.push_back("\n");
}

double square$threadAactionPrintSquare(double x)
{
  return (x * x); //returntype: Number
}


int main()
{
rendezvousCounter.store(0);

threadA$d = 0.0;
threadA$i = 0;
std::thread tthreadA (threadA);
while(true)
{
std::this_thread::sleep_for(std::chrono::seconds(1));
}
}