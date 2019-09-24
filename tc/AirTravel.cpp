#include <vector>
#include <string>
#include <math.h>
#include <sstream>
#include <limits>
#include <iterator>
#include <algorithm>
#include <cassert>
#include <iostream>

class AirTravel {
	private:
		static constexpr double radius = 4000.0;
		static constexpr double mul = 3.14159265359;
		constexpr double distance(double lat1, double lat2, double lon1, double lon2) {
			return radius * acos(sin(lat1 * mul) * sin(lat2 * mul) + cos(lat1 * mul) * cos(lat2* mul) * cos(lon1 * mul - lon2 * mul));
		}
	public:
		double shortestTrip(std::vector<int> latitude, std::vector<int> longitude, std::vector<std::string> canTravel, int origin, int destination) {
			const int N = latitude.size();
			std::vector<std::vector<double> > d(N);
			std::vector<std::vector<std::string> > G;

			for (auto s: canTravel) {
				std::stringstream ss(s);
				std::istream_iterator<std::string> it(ss);
				std::istream_iterator<std::string> end;
				G.push_back(std::vector<std::string>(it, end));
			}

			assert(G.size() == N);
			
			for (auto &v: d) v.resize(N, std::numeric_limits<double>::max());

			for (auto i = 0; i < N; ++i)
				for (auto j = 0; j < G[i].size(); ++j) {
					auto k = std::stoi(G[i][j]);
					if (i != k) d[i][k] = std::min(distance(latitude[i], latitude[k], longitude[i], longitude[k]), d[i][k]);
				}
			for (auto i = 0; i < N; ++i)
				for (auto j = 0; j < N; ++j)	
					for (auto k = 0; k < N; ++k) {
						if ((d[i][j] > d[i][k] + d[k][j])) d[i][j] = d[i][k] + d[k][j];
					}
			if (d[origin][destination] < std::numeric_limits<double>::max()) return d[origin][destination]; else return -1;
		}
};

int main() {
	std::cout << (new AirTravel)->shortestTrip(std::vector<int>({0, 0, 70}), std::vector<int>({90, 0, 45}), std::vector<std::string>({{"1 2", "0 2", "0 1"}}), 0, 1) << "\n";
}