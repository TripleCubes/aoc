#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <cmath>

template<typename T>
using DynArr = std::vector<T>;
typedef std::string Str;

void read_file(Str &result, const Str &path) {
	std::ifstream i(path);
	std::stringstream ss;
	ss << i.rdbuf();
	result = ss.str();
}

bool get_is_white_space(char c) {
	return c == ' ' || c == '\n' || c == '\r';
}

void to_num_list(DynArr<int> &result, const Str &str) {
	Str word;

	for (int i = 0; i < (int)str.length(); i++) {
		char c = str[i];
		bool is_white_space = get_is_white_space(c);

		if (is_white_space && word.length() != 0) {
			result.push_back(std::stoi(word));
			word.clear();
			continue;
		}

		if (is_white_space) {
			continue;
		}

		word.push_back(c);
	}

	if (word.length() != 0) {
		result.push_back(std::stoi(word));
	}
}

void to_2_lists(DynArr<int> &left_list, DynArr<int> &right_list,
const DynArr<int> &num_list) {
	for (int i = 0; i < (int)num_list.size(); i += 2) {
		left_list.push_back(num_list[i]);
		right_list.push_back(num_list[i + 1]);
	}
}

int appear_times(int n, const DynArr<int> &list) {
	int count = 0;
	for (int i = 0; i < (int)list.size(); i++) {
		if (list[i] == n) {
			count++;
		}
	}

	return count;
}

int get_result_0(const Str &path) {
	Str in;
	read_file(in, path);

	DynArr<int> num_list;
	to_num_list(num_list, in);

	DynArr<int> left_list;
	DynArr<int> right_list;
	to_2_lists(left_list, right_list, num_list);

	std::sort(left_list.begin(), left_list.end());
	std::sort(right_list.begin(), right_list.end());

	int result = 0;
	for (int i = 0; i < (int)left_list.size(); i++) {
		int l = left_list[i];
		int r = right_list[i];
		result += std::abs(l - r);
	}

	return result;
}

int get_result_1(const std::string &path) {
	Str in;
	read_file(in, path);

	DynArr<int> num_list;
	to_num_list(num_list, in);

	DynArr<int> left_list;
	DynArr<int> right_list;
	to_2_lists(left_list, right_list, num_list);

	int result = 0;
	for (int i = 0; i < (int)left_list.size(); i++) {
		int left_num = left_list[i];
		result += left_num * appear_times(left_num, right_list);
	}

	return result;
}

int main() {
	std::cout << get_result_0("../input/example.txt") << std::endl;
	std::cout << get_result_0("../input/input.txt") << std::endl;
	std::cout << get_result_1("../input/example.txt") << std::endl;
	std::cout << get_result_1("../input/input.txt") << std::endl;
	return 0;
}

