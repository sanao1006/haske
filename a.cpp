#include <bits/stdc++.h>
using namespace std;
int main() {
  int h, w, n;
  cin >> h >> w >> n;
  vector<int> a(n), b(n);
  vector<int> vh, vw;

  for (int i = 0; i < n; ++i) {
    cin >> a[i] >> b[i];
    vh.push_back(a[i]);
    vw.push_back(b[i]);
  }

  sort(vh.begin(), vh.end());
  vh.erase(unique(vh.begin(), vh.end()), vh.end());

  sort(vw.begin(), vw.end());
  vw.erase(unique(vw.begin(), vw.end()), vw.end());

  for (int i = 0; i < n; ++i) {
    cout << lower_bound(vh.begin(), vh.end(), a[i]) - vh.begin() + 1 << ' ';
    cout << lower_bound(vw.begin(), vw.end(), b[i]) - vw.begin() + 1 << '\n';
  }
}