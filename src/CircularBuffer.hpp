
#include <list>
#include <ostream>
#include <sstream>
#include <string>
#include <time.h>

class CircularBuffer {
    public:
        CircularBuffer(size_t maxSizeIn) : maxSize(maxSizeIn), entriesToPrint(0), ostreamForPrinting(NULL), data(), firstEntry(true) {}

        void addLine(std::string stringIn) {
            // compute timeDiff and update prevTime
            time_t timeDiff;
            time_t currTime;
            time(&currTime);
            if (!firstEntry) {
                timeDiff = currTime - prevTime;
            } else {
                timeDiff = 0;
                firstEntry = false;
            }
            prevTime = currTime;

            // Add a line to the string when a significant amount of time has
            // elapsed.
            std::ostringstream buffer;
            if (timeDiff > timeThreshold) {
                buffer << "[" << std::dec << timeDiff << " seconds elapsed]" << std::endl;
            }
            buffer << stringIn;

            // Either print the string or add it to a buffer
            if (entriesToPrint > 0) {
                (*ostreamForPrinting) << buffer.str() << std::endl;
                entriesToPrint--;
            } else {
                data.push_back(buffer.str());
                if (data.size() > maxSize) {
                    data.pop_front();
                }
            }
        }

        void printToOStream(std::ostream * o, size_t extraEntriesToPrint) {
            // compute timeDiff and update prevTime (if firstEntry == false)
            time_t currTime;
            time_t timeDiff;
            time(&currTime);
            if (!firstEntry) {
                timeDiff = currTime - prevTime;
                prevTime = currTime;
            } else {
                timeDiff = 0;
            }
            // if this is the first entry, nothing will be done

            // dump the current circular buffer
            while (data.size() > 0) {
                (*o) << data.front() << std::endl;
                data.pop_front();
            }
            if (timeDiff > timeThreshold) {
                (*o) << "[" << std::dec << timeDiff << " seconds elapsed]" << std::endl;
            }

            // if extraEntriesToPrint > 0, then future lines will be printed
            // as they are added
            ostreamForPrinting = o;
            entriesToPrint = extraEntriesToPrint;
        }

    private:
        size_t maxSize;
        size_t entriesToPrint;
        std::ostream *ostreamForPrinting;
        std::list<std::string> data;
        bool firstEntry;
        time_t prevTime;

        const time_t timeThreshold = 3;
};
