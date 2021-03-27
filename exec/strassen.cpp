#include <fstream>
#include <string>
#include <vector>

struct m {
  int h;
  int w;
  std::vector<int> mat;
  m(int h, int w, std::vector<int> mat) {
    this->h = h;
    this->w = w;
    this->mat = mat;
  }
};

int get(m m, int row, int col) { return m.mat[row * m.w + col]; }

m mult_normal(m m1, m m2) {
  std::vector<int> new_m;
  for (int row = 0; row < m1.h; row++) {
    for (int col = 0; col < m2.w; col++) {
      int sum = 0;
      for (int i = 0; i < m1.w; i++) {
        sum += get(m1, row, i) + get(m2, i, col);
      }
      new_m.push_back(sum);
    }
  }
  return m(m1.h, m2.w, new_m);
}

m read_file(std::string file) {
  std::fstream newfile;
  newfile.open(file, std::ios::in);
  if (newfile.is_open()) { // checking whether the file is open
    std::string tp;
    while (getline(newfile,
                   tp)) { // read data from file object and put it into string.
      cout << tp << "\n"; // print the data of the string
    }
    newfile.close(); // close the file object.
  }
}