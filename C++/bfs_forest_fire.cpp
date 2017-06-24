#include <iostream>
#include <fstream>
#include <vector>
#include <queue>
using namespace std;
//use nspace <std>;

const int dx[8] = {0 , 0,-1, 1, 1, 1, -1, -1};
const int dy[8] = {-1, 1, 0, 0, 1, -1, 1, -1};
std::queue<int> qx;
std::queue<int> qy;
std::vector< vector<int> > use;
int lastx, lasty;

void bfs(int startx, int starty, int N, int M){
    use[startx][starty] = 1;
    lastx = startx;
    lasty = starty;
    qx.pop();
    qy.pop();

    for(int k = 0; k < 8; k++){
        int nx = startx + dx[k];
        int ny = starty + dy[k];
        if(nx < 0 || nx >= N || ny < 0 || ny >= M){
            continue;
        }
        if(!use[nx][ny]){
            use[nx][ny] = 1;
            qx.push(nx);
            qy.push(ny);
        }

    }
}

int main(void){
    int N, M, K, x, y;
    std::ifstream file("input.txt");
    std::ofstream out("output.txt");
    file >> N  >> M  >> K;
    printf("%d %d %d \n", N, M, K);
    /*
    use.resize(N);
    for(int i = 0; i < M; i++){
        use[]
    }
    */
    for(int i = 0; i < K; i++){
        file >> x >> y;
        printf("%d %d \n", x, y);
        qx.push(x);
        qy.push(y);
    }
    use.resize(N);
    for(int i =0;i<N;i++){
        use[i].resize(M);
    }
    while(!qx.empty()){
        bfs(qx.front(), qy.front(), N, M);
    }
    printf("%d %d \n", lastx, lasty);
    out << lastx << lasty;
}



/*
void dfs(int x, int y);
std::vector<std::vector<int> > a;
std::vector<int> use;
const int dx[u] = {0,0,-1,1};
const int dy[u] = {-1,1,0,0};

void dfs(int x, int y){
    use[x][y] = 1;
    for(int r = 0; r<k; r++){
        int nx = x + dx[k];
        int ny = y + dy[k];
        if(nx < 0 || nx >= N || ny < 0 || ny >= M){
            continue;
        }
        if(!use[nx][ny]){
            dfs(nx, ny);
        }
    }
}
*/