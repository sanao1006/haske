#pragma GCC optimize("Ofast")
#include <bits/stdc++.h>
#define REP(i, n) for (int i = 0; i < int(n); i++)
template <typename A, typename B>
inline bool chmax(A &a, B b)
{
    if (a < b)
    {
        a = b;
        return true;
    }
    return false;
}
using namespace std;
using ll = long long;
short dp[4005][4005];
int main()
{
    ios::sync_with_stdio(false);
    string s, t;
    cin >> s >> t;
    ll n = s.length();
    ll m = t.length();
    ll ans = 0;
    REP(i, n)
    {
        REP(j, m)
        {
            if (s[i] == t[j])
            {
                if (i - 1 < 0 || j - 1 < 0)
                {
                    dp[i][j] = 1;
                }
                else
                {
                    dp[i][j] = dp[i - 1][j - 1] + 1;
                }
            }
            chmax(ans, dp[i][j]);
        }
    }
    cout << ans << endl;
}