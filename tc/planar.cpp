#include <iostream>
#include <algorithm>
#include <queue>
#include <vector>
#include <queue>
#include <limits>

int n, m;

std::vector<int> x, y;

struct Edge {
	int v, f, c;
	Edge* rev;
};

std::vector<std::vector<Edge> > G;

std::vector<Edge*> bfs() {	
	std::vector<int> prev(n + 2, -1);
	std::vector<int> id(n + 2, -1);
	std::queue<int> Q;
	Q.push_back(0);
	
	while (!Q.empty()) {
		auto v = Q.top();
		Q.pop();
		id[0] = -5;
		for (auto i = G[v].begin(); i != G[v].end(); ++i) {
			
		}
	}
}

void push(std::vector<Edge*> ev) {
	
}

int main() {	
	std::cin >> n >> m; 
	G.resize(n + 2);
	x.resize(n + 2);
	for (auto i = 1; i <= n; ++i) {
		int a, b; std::cin >> a >> b;
		x[i] = a; y[i] = b;
	}
	for (auto i = 0; i < m; ++i) {
		int a, b; std::cin >> a >> b;
		std::cout << a << "->" << b << "\n";
		auto e1 = Edge {b, 0, std::numeric_limits<int>::max()};
		auto e2 = Edge {a, 0, std::numeric_limits<int>::max()};
		e1.rev = &e2; e2.rev = &e1;
		G[a].push_back(e1);
		G[b].push_back(e2);
	}
	
	int xmin = *std::min_element(x.begin(), x.end()), ymin = *std::min_element(y.begin(), y.end());
	int xmax = *std::max_element(x.begin(), x.end()), ymax = *std::max_element(y.begin(), y.end());
	
	auto epsilon = 1;
	//(xmin - epsilon, 0.5 * (ymax + ymin) 
	x[0] = xmin - epsilon; y[0] = 0.5 * (ymax + ymin);
	
	//(xmax + epsilon, 0.5 * (ymax + ymin))
	x[n] = xmin + epsilon; y[n] = 0.5 * (ymax + ymin);
	
	for (auto i = 1; i < n; ++i) {
		auto e1 = Edge {i, 0, 1};
		auto e2 = Edge {0, 0, 1};
		e1.rev = &e2;
		e2.rev = &e1;
		G[0].push_back(e1);
		G[i].push_back(e2);
		
	}
	for (auto i = 1; i < n; ++i) {
		auto e1 = Edge {n, 0, 1};
		auto e2 = Edge {0, 0, 1};
		e1.rev = &e2;
		e2.rev = &e1;
		G[i].push_back(e1);
		G[n].push_back(e2);
	}
	return 0;
}