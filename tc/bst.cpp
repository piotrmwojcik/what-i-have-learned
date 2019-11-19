#include <vector>
#include <iostream>

struct Node {
	Node *left, *right, *parent;
	int val;
	Node(int v):left(0), right(0), parent(0) {}
};

class BST {
private:
	Node* root;
	Node* find_(int x, Node *n) {
		if (!root) return 0;
		if (n->val == x) return n;
		if (x < n->val) 
			return find_(x, n->left);
		else return find_(x, n->right); 	
	}
	void inorder_(Node *n) {
		if (!n) return;
		inorder_(n->left);
		std::cout << n->val << " ";
		inorder_(n->right);
	}
public:
	BST():root(0) {}
	Node* find(int x) {
		return find_(x, root); 
	}
	void inorder() {
		inorder_(root);	
	}
	void insert(int x) {
		Node *n = new Node(x);
		if (find(x)) return;
		
	}
};

int main() {
	BST* bst = new BST();
	std::vector<int> v = {1, 2, 0, 5, 4, 6};
	for (auto i: v) bst->insert(i); 
	BST->inorder();
	return 0;
}
