#include <iostream>
#include <algorithm>
#include <queue>
#include <vector>
#include <queue>
#include <cassert>
#include <limits>

int n, m;

std::vector<int> x, y;

struct Edge {
	int v, f, c;
	Edge* rev;
};

std::vector<std::vector<Edge> > G;

std::vector<Edge*> bfs(int start) {	
	std::vector<int> prev(n + 2, -1);
	std::vector<Edge*> id(n + 2, NULL);
	std::vector<int> visited(n + 2, false);
	std::queue<int> Q;
	Q.push(start);
	visited[start] = true;
	while (!Q.empty()) {
		auto v = Q.front();
		Q.pop();
		if (id[1]) std::cout << "###" << id[1]->v << "\n";
		if (v == n + 1) break;
		for (auto i = G[v].begin(); i != G[v].end(); ++i) {
			auto e = *i;
			if (e.c - e.f <= 0) continue;
			if (visited[e.v]) continue;
			Q.push(e.v);
			visited[e.v] = true;
			std::cout << "edge to: " << e.v << " from " << v << "\n"; 
			prev[e.v] = v;
			id[e.v] = &e;
			std::cout << "id[" << e.v << "] == " << id[e.v]->v  << "\n";
			std::cout << "prev[" << e.v << "] == " << prev[e.v] << " \n"; 
		}
	}
	if (!visited[n + 1]) return std::vector<Edge*> ();
	std::vector<Edge*> ret;
	auto k = n + 1;	
	while (id[k]) {
		//assert(k == id[k]->v);
		std::cout << ":: k == " << k << " " << id[k]->v << "\n";
		ret.push_back(id[k]);
		k = prev[k];
	}
	return ret;
}

void printGraph() {
	for (auto i=0;i<=n+1;++i) {
		std::cout << "vertex " << i << ": \n";
			for (auto j=0;j<G[i].size();++j)
				std::cout << G[i][j].v << " "; 
		std::cout << "\n";
	}
}


void flow() {
	std::vector<Edge*> augPath = bfs(0);
	std::cout << "flow\n";
	//while (!(augPath = bfs(0)).empty()) {
		for (auto i = 0; i < augPath.size(); ++i) {std::cout << augPath[i]->v << " ";}
	//}
	std::cout << std::endl;
}

int main() {	
	std::cin >> n >> m; 
	G.resize(n + 2);
	x.resize(n + 2);
	y.resize(n + 2);	

	for (auto i = 0; i < m; ++i) {
		int a, b; std::cin >> a >> b;
		std::cout << a << "->" << b << "\n";
		auto e1 = Edge {b, 0, std::numeric_limits<int>::max()};
		auto e2 = Edge {a, 0, std::numeric_limits<int>::max()};
		e1.rev = &e2; e2.rev = &e1;
		G[a].push_back(e1);
		G[b].push_back(e2);
	}

	for (auto i = 1; i <= n; ++i) {
		int a, b; std::cin >> a >> b;
		x[i] = a; y[i] = b;
	}
	
	int xmin = *std::min_element(x.begin(), x.end()), ymin = *std::min_element(y.begin(), y.end());
	int xmax = *std::max_element(x.begin(), x.end()), ymax = *std::max_element(y.begin(), y.end());
	
	auto epsilon = 1;
	//(xmin - epsilon, 0.5 * (ymax + ymin) 
	x[0] = xmin - epsilon; y[0] = 0.5 * (ymax + ymin);
	
	//(xmax + epsilon, 0.5 * (ymax + ymin))
	x[n] = xmax + epsilon; y[n] = 0.5 * (ymax + ymin);
	
	std::cout << x[0] << "  " << y[0] << std::endl;
	std::cout << x[n] << "  " << y[n] << std::endl;
	
	for (auto i = 1; i < n; ++i) {
		auto e1 = Edge {i, 0, 1};
		auto e2 = Edge {0, 0, 0};
		e1.rev = &e2;
		e2.rev = &e1;
		G[0].push_back(e1);
		G[i].push_back(e2);
		
	}
	for (auto i = 1; i <= n; ++i) {
		auto e1 = Edge {n+1, 0, 1};
		auto e2 = Edge {i, 0, 0};
		e1.rev = &e2;
		e2.rev = &e1;
		G[i].push_back(e1);
		G[n+1].push_back(e2);
	} 
	//debug printGraph();
	flow();
	return 0;
}
