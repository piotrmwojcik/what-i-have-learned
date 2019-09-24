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
	std::vector<int> visited(n + 2, false);
	std::queue<int> Q;
	visited[0] = true;
	Q.push_back(0);
	
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
		auto 
		G[a].push_back(Edge {b, 0, std::numeric_limits<int>::max()});
		G[b].push_back(Edge {a, 0, std::numeric_limits<int>::max()});
	}
	
	int xmin = *std::min_element(x.begin(), x.end()), ymin = *std::min_element(y.begin(), y.end());
	int xmax = *std::max_element(x.begin(), x.end()), ymax = *std::max_element(y.begin(), y.end());
	
	auto epsilon = 1;
	//(xmin - epsilon, 0.5 * (ymax + ymin) 
	x[0] = xmin - epsilon; y[0] = 0.5 * (ymax + ymin);
	
	//(xmax + epsilon, 0.5 * (ymax + ymin))
	x[n] = xmin + epsilon; y[n] = 0.5 * (ymax + ymin);
	
	for (auto i = 1; i < n; ++i) G[0].push_back(Edge {i, 0, 1});
	for (auto i = 1; i < n; ++i) G[i].push_back(Edge {n, 0, 1});
	return 0;
}