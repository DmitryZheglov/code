#include <iostream>
#include <fstream>
#include <vector>
using namespace std;

std::vector<std::vector<int> > a;
std::vector<int> use;

void dfs(int v);

void dfs(int v){
	use[v] = true;
	for(int i = 0; i < a[v].size();i++){
		int t0 = a[v][i];
		if(use[t0]!=true){
			dfs(t0);
		}
	}
}

int main(void){
	int n,m,v,ans = 0;
	std::ifstream file("components-count.in");
	std::ofstream out("components-count.out");
	file >> n;
	a.resize(n);

	for(int i = 0;i < n; i++){
		use.push_back(false);
	}

	for(int i = 0; i < n; ++i){
		for(int j = 0;j<n;j++){
			file >> v;
			if(v!=0){ 
				a[i].push_back(j);
			}
		}
	}
	file.close();
	for(int i = 0; i < n; i++){
		if(use[i]!=true){
			dfs(i);
			ans = ans + 1;
		}
	}
	out << ans;
	//std::cout << ans << std::endl;
	return 0;
}



