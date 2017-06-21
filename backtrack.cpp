#include <iostream>
#include <ctime>
namespace ListCombs {

	int solution[5];

	void printSolution()
	{
		for (auto i = 0; i < 5; ++i) {
			std::cout << solution[i] << ' ';
		}
		std::cout << std::endl;
	}

	void backTrack(int n)
	{
		if (n == 2) {
			printSolution();
			return;
		}
		for (auto i = 0; i != 10; i++) {
			solution[n] = i;
			backTrack(n + 1);
		}
	}
};


namespace Permutaion {
	const int MAX = 20;

	int solution[MAX];
	bool used[MAX] = { 0 };

	void printSolution()
	{
		for (auto i = 0; i != MAX; ++i) {
			std::cout << solution[i] << ' ';
		}
		std::cout << std::endl;
	}

	void permutation(int k, int n)
	{
		if (k == n) {
			//printSolution();
			return;
		}
		for (auto v = 0; v != n; v++) {
			if (!used[v]) {
				used[v] = true;
				solution[k] = v;
				permutation(k + 1, n);
				used[v] = false;
			}
		}
	}

	void init()
	{
		for (auto i = 0; i < MAX; ++i) {
			used[i] = false;
		}
	}
};



namespace Permutaion2 {
	const int MAX = 20;

	char solution[MAX];
	bool filled[MAX];

	void printSolution()
	{
		for (auto i = 0; i != MAX; ++i) {
			std::cout << char(solution[i]) << ' ';
		}
		std::cout << std::endl;
	}

	void permutation(int v, int n)
	{
		if (v == n) {
			//printSolution();
			return;
		}
		for (auto i = 0; i != n; i++) {
			if (!filled[i]) {
				filled[i] = true;
				solution[i] = v + 'a';
				permutation(v + 1, n);
				filled[i] = false;
			}
		}
	}

	void init()
	{
		for (auto i = 0; i < MAX; ++i) {
			filled[i] = false;
		}
	}
};

namespace StrPermutaion {
	const int MAX = 80;
	int array[128]; //ASCII 字符出现次数
	char solution[MAX];

	void printSolution()
	{
		for (auto i = 0; i != MAX; ++i) {
			std::cout << char(solution[i]) << ' ';
		}
		std::cout << std::endl;
	}

	void permutation(int k, int n)
	{
		if (k == n) {
			printSolution();
			return;
		}

		for (auto i = 0; i < 128; ++i) {
			if (array[i] > 0) {
				--array[i];
				solution[k] = i;
				permutation(k + 1, n);
				++array[i];
			}
		}
	}
	void init(char *str) {
		memset(array, 0, 128);
		int i = 0;
		while (str[i] != '\0') {
			++array[str[i]];
			++i;
		}
	}
}

namespace AllSubsets {
	const int MAX = 5;
	int subset[MAX];
	int input[5] = { 1,2,3,4,5 };

	//in fact init is not necessary,it can be overwritten with meaningful values.
	void init() {
		memset(subset, 0, MAX);
	}

	void printSolution(int N)
	{
		for (auto i = 0; i != N; ++i) {
			std::cout << subset[i] << ' ';
		}
		std::cout << std::endl;
	}


	void backTrack(int n, int N)
	{
		if (n == 5) {
			printSolution(N);
			return;
		}


		subset[N] = input[n];
		backTrack(n + 1, N + 1);

		backTrack(n + 1, N);

	}
}


namespace EightQueen {
	const int size = 4;
	void naiveBackTrack(int x, int y);
	bool solution[size][size];
	void clearSolution()
	{
		for (auto i = 0; i != size; ++i) {
			for (auto j = 0; j != size; ++j) {
				solution[i][j] = false;
			}
		}
	}
	void printSolution()
	{
		std::cout << "------------------" << std::endl;
		for (auto i = 0; i != size; ++i) {
			for (auto j = 0; j != size; ++j) {
				if (solution[i][j]) {
					std::cout << i << "," << j << std::endl;
				}
			}
		}
		std::cout << "------------------" << std::endl;
	}

	void naiveBackTrack(int x, int y)
	{
		if (y == size) {
			++x;
			y = 0;
		}
		if (x == size) {
			printSolution();
			return;
		}

		solution[x][y] = true;
		naiveBackTrack(x, y + 1);

		solution[x][y] = false;
		naiveBackTrack(x, y + 1);
	}


	bool mx[size], my[size];
	bool md1[size * 2 - 1], md2[size * 2 - 1];

	//mark intruding lines of new queen at (x,y)
	void markLines(int x, int y)
	{
		auto d1 = (x + y) % (size * 2 - 1);
		auto d2 = (x - y + (size * 2 - 1)) % (size * 2 - 1);
		mx[x] = true;
		my[y] = true;
		md1[d1] = true;
		md2[d2] = true;

	}

	//make the lines available when remove queen at (x,y)
	void wipeLines(int x, int y)
	{
		auto d1 = (x + y) % (size * 2 - 1);
		auto d2 = (x - y + (size * 2 - 1)) % (size * 2 - 1);

		mx[x] = false;
		my[y] = false;
		md1[d1] = false;
		md2[d2] = false;
	}

	bool isAvailable(int x, int y)
	{
		auto d1 = (x + y) % (size * 2 - 1);
		auto d2 = (x - y + (size * 2 - 1)) % (size * 2 - 1);
		return !mx[x] && !my[y] && !md1[d1] && !md2[d2];
	}

	void backTrack(int x, int y)
	{
		if (y == size) {
			++x;
			y = 0;
		}
		if (x == size) {
			printSolution();
			return;
		}
		if (isAvailable(x, y)) {//adding three lines compared with naive backTrack.
			markLines(x, y);	//record state
			solution[x][y] = true;
			backTrack(x, y + 1);
			wipeLines(x, y);	//recover state
		}
		solution[x][y] = false;
		backTrack(x, y + 1);
	}

	int RowSolution[size];

	void printRowSolution()
	{
		std::cout << "------------------" << std::endl;
		for (auto i = 0; i != size; ++i) {
			std::cout << i << "," << RowSolution[i] << std::endl;
		}

		std::cout << "------------------" << std::endl;
	}

	//solution[x]=y means put queen at (x,y)
	void rowBackTrackNaive(int x)
	{
		if (x == size) {
			printRowSolution();
			return;
		}

		for (auto y = 0; y < size; ++y) {
			RowSolution[x] = y;
			rowBackTrackNaive(x + 1);
		}
	}
	void rowBackTrack(int x)
	{
		if (x == size) {
			printRowSolution();
			return;
		}

		for (auto y = 0; y < size; ++y) {
			if (isAvailable(x, y)) {
				markLines(x, y);
				RowSolution[x] = y;
				rowBackTrack(x + 1);
				wipeLines(x, y);
			}
		}
	}
}

namespace Sudoku {
	const int size = 9;
	int solution[size][size];
	void printSolution()
	{
		std::cout << "------------------" << std::endl;
		for (auto i = 0; i != size; ++i) {
			for (auto j = 0; j != size; ++j) {
				std::cout << solution[i][j] << ",";
			}
			std::cout<< std::endl;
		}
		std::cout << "------------------" << std::endl;
	}

	void naiveBackTrack(int x, int y)
	{
		if (y == size) {
			++x;
			y = 0;
		}
		if (x == size) {
			printSolution();
		}
		for (auto n = 1; n != size + 1; ++n) {
			solution[x][y] = n;
			naiveBackTrack(x, y + 1);
		}
	}

	bool mx[9][10], my[9][10], mg[3][3][10];

	//is available to put n at (x,y)
	bool isAvailable(int x,int y,int n) 
	{
		return !mx[x][n] && !my[y][n] 
			&& !mg[x / 3][y / 3][n];
	}
	void markState(int x, int y, int n)
	{
		mx[x][n] = true;
		my[y][n] = true;
		mg[x / 3][y / 3][n] = true;
	}
	void wipeState(int x, int y, int n)
	{
		mx[x][n] = false;
		my[y][n] = false;
		mg[x / 3][y / 3][n] = false;
	}


	void allAnswerBackTrack(int x, int y)
	{
		if (y == size) {
			++x;
			y = 0;
		}
		if (x == size) {
			printSolution();
		}
		for (auto n = 1; n != size + 1; ++n) {
			if (isAvailable(x, y, n)) {
				markState(x, y, n);
				solution[x][y] = n;
				allAnswerBackTrack(x, y + 1);
				wipeState(x, y, n);
			}
		}
	}

	//preInput values,the user have to fill in it with 
	//reasonable inputs.
	int board[size][size];
	void initialize()
	{
		for (int x = 0; x != size; ++x){
			for (int y = 0; y != size; ++y) {
				if (board[x][y]) {
					int n = board[x][y];
					markState(x, y, n);
					solution[x][y] = board[x][y];
				}
			}
		}
	}
	void backTrack(int x, int y)
	{
		if (y == size) {
			++x;
			y = 0;
		}
		if (x == size) {
			printSolution();
		}
		if (board[x][y]) {
			backTrack(x, y + 1);
			return;
		}

		for (auto n = 1; n != size + 1; ++n) {
			if (isAvailable(x, y, n)) {
				markState(x, y, n);
				solution[x][y] = n;
				backTrack(x, y + 1);
				wipeState(x, y, n);
			}
		}
	}
}


int main()
{
	ListCombs::backTrack(0);

	auto n = 2;
	auto c1 = std::clock();
	Permutaion::init();
	Permutaion::permutation(0, n);
	auto c2 = std::clock();
	std::cout << c2 - c1 << std::endl;

	auto c3 = std::clock();
	Permutaion2::init();
	Permutaion2::permutation(0, n);
	auto c4 = std::clock();
	std::cout << c4 - c3 << std::endl;

	StrPermutaion::init("aabc");
	StrPermutaion::permutation(0, 4);


	AllSubsets::init();
	AllSubsets::backTrack(0, 0);

	EightQueen::backTrack(0,0);
	EightQueen::rowBackTrack(0);
	
	Sudoku::board[0][0] = 1;
	Sudoku::board[1][1] = 2;

	Sudoku::board[2][2] = 3;
	Sudoku::initialize();
	Sudoku::backTrack(0, 0);

	return 0;
}
