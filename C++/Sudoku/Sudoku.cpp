#include <iostream>
#include <cstdarg>
#include <vector>
#include <stack>
#include <string>
#include <ctime>
#include <fstream>
using namespace std;

class Sudoku
{
private:
	int _Num[81];
	int _V[10];
	string _S;
	bool _HasString;

	int Get1Count(int Value)
	{
		int C = 0;
		while (Value > 0)
		{
			Value &= (Value - 1);
			C += 1;
		}
		return C;
	}

	int RemoveNum(int Row, int Col, int Num2)
	{
		int Index = Row * 9 + Col;
		if (_Num[Index] > 0)
		{
			_Num[Index] &= Num2;
		}
		return _Num[Index];
	}

	bool SetNumPri(int Row, int Col, int Num)
	{
		if ((_V[Num] & _Num[Row * 9 + Col]) == 0)
		{
			return false;
		}
		_Num[Row * 9 + Col] = -(Num + 1);
		Num = _V[9] - _V[Num];

		int I, J;

		for (I = 0; I <= 8; ++I)
		{
			if (RemoveNum(I, Col, Num) == 0 || RemoveNum(Row, I, Num) == 0)
			{
				return false;
			}
		}

		int R1 = (Row / 3) * 3;
		int C1 = (Col / 3) * 3;

		for (I = R1; I <= R1 + 2; ++I)
		{
			for (J = C1; J <= C1 + 2; ++J)
			{
				if (RemoveNum(I, J, Num) == 0)
				{
					return false;
				}
			}
		}
		return true;
	}

	bool SetNumPri(int Index, int Num2)
	{
		int Row = Index / 9;
		int Col = Index % 9;
		int I;
		for (I = 0; I <= 8; ++I)
		{
			if (_V[I] == Num2)
			{
				break;
			}
		}
		return SetNumPri(Row, Col, I);
	}

	int GetOnly()
	{
		int I, J, K;
		int P[9];

		for (I = 0; I <= 8; ++I)
		{
			for (K = 0; K <= 8; ++K)
			{
				P[K] = -1;
			}

			for (J = 0; J <= 8; ++J)
			{
				if (_Num[I * 9 + J] > 0)
				{
					for (K = 0; K <= 8; ++K)
					{
						if ((_Num[I * 9 + J] & _V[K]) == _V[K])
						{
							P[K] = P[K] == -1 ? I * 9 + J : -2;
						}
					}
				}
			}

			for (K = 0; K <= 8; ++K)
			{
				if (P[K] >= 0)
				{
					return P[K] * 10 + K;
				}
			}
		}

		for (I = 0; I <= 8; ++I)
		{
			for (K = 0; K <= 8; ++K)
			{
				P[K] = -1;
			}

			for (J = 0; J <= 8; ++J)
			{
				if (_Num[J * 9 + I] > 0)
				{
					for (K = 0; K <= 8; ++K)
					{
						if ((_Num[J * 9 + I] & _V[K]) == _V[K])
						{
							P[K] = P[K] == -1 ? J * 9 + I : -2;
						}
					}
				}
			}

			for (K = 0; K <= 8; ++K)
			{
				if (P[K] >= 0)
				{
					return P[K] * 10 + K;
				}
			}
		}

		int S;
		for (I = 0; I <= 8; ++I)
		{
			for (K = 0; K <= 8; ++K)
			{
				P[K] = -1;
			}

			for (J = 0; J <= 8; ++J)
			{
				S = ((I / 3) * 3 + J / 3) * 9 + (I % 3) * 3 + J % 3;

				if (_Num[S] > 0)
				{
					for (K = 0; K <= 8; ++K)
					{
						if ((_Num[S] & _V[K]) == _V[K])
						{
							P[K] = P[K] == -1 ? S : -2;
						}
					}
				}
			}

			for (K = 0; K <= 8; ++K)
			{
				if (P[K] >= 0)
				{
					return P[K] * 10 + K;
				}
			}
		}

		return -1;
	}

	int FindMinCell()
	{
		int I = 0, C, S, tP = -1, tMin = 20;

		do {
			do {
				if (_Num[I] > 0)
				{
					C = Get1Count(_Num[I]);
					if (C == 1)
					{
						if (!SetNumPri(I, _Num[I]))
						{
							return -2;
						}

						if (I == tP)
						{
							tP = -1;
							tMin = 20;
						}

						I = -1;
					}
					else if (C < tMin)
					{
						tP = I;
						tMin = C;
					}
				}
				I += 1;
			} while (I <= 80);

			if (tP == -1)
			{
				return -1;
			}

			S = GetOnly();

			if (S > 0)
			{
				int S2 = S / 10;
				int S3 = S % 10;

				if (!(SetNumPri(S2, _V[S3])))
				{
					return -2;
				}

				I = 0;
				tP = -1;
				tMin = 20;
			}
		} while (I <= 80);

		return tP;
	}

	void RestoreNum(vector<int> L)
	{
		int I;
		for (I = 0; I <= 80; ++I)
		{
			_Num[I] = L[I];
		}

		AppendString("Restore Matrix");
	}

	int GetIndexOfNum(int Num, int Index)
	{
		int I, K = 0;
		for (I = 0; I <= 8; ++I)
		{
			if ((_V[I] & Num) != 0)
			{
				K += 1;
				if (K == Index)
				{
					return I + 1;
				}
			}
		}
		return -1;
	}

	int FindNextK(stack<vector<int>>& Q, vector<int>& L, int K, int Index)
	{
		int J = GetIndexOfNum(_Num[K], Index);
		if (J == -1)
		{
			return -2;
		}

		SetNumPri(K, _V[J - 1]);
		AppendString("Stack Push " + to_string(Q.size() + 1), false);
		AppendString("SetNum MayBe " + IndexToXY(K));

		L.push_back(Index);
		L.push_back(K);
		Q.push(L);

		K = FindMinCell();
		return K;
	}

	string ReturnNumString(int Num)
	{
		int I;
		string S = "";

		if (Num < 0)
		{
			S = "#" + to_string(-Num);
		}
		else
		{
			for (I = 0; I <= 8; ++I)
			{
				if ((_V[I] & Num) != 0)
				{
					S.append(to_string(I + 1));
				}
			}
		}
		int pad_size = 10;
		S.insert(S.size(), pad_size - S.size(), ' ');
		return S;
	}

	string ReturnMatrix()
	{
		int I, J;
		string S = "";

		for (I = 0; I <= 8; ++I)
		{
			for (J = 0; J <= 8; ++J)
			{
				S.append(ReturnNumString(_Num[I * 9 + J]));
			}
			S.append("\n");
		}
		return S;
	}

	void AppendString(string Text, bool AppendMatrix = true)
	{
		if (!_HasString)
		{
			return;
		}
		_S.append(Text);
		_S.append("\n\n");
		if (AppendMatrix)
		{
			_S.append(ReturnMatrix());
			_S.append("\n");
		}
		return;
	}

	string IndexToXY(int Index)
	{
		return to_string(Index / 9 + 1) + "-" + to_string(Index % 9 + 1) + " Num: " + to_string(-_Num[Index]);
	}

public:
	Sudoku(bool HasString = true)
	{
		_V[0] = 1;
		for (int I = 1; I <= 8; ++I)
		{
			_V[I] = _V[I - 1] * 2;
		}
		_V[9] = 511;

		for (int I = 0; I <= 80; ++I)
		{
			_Num[I] = _V[9];
		}
		_HasString = HasString;
		_S = "";
	}

	bool SetNum(int Row, int Col, int Num)
	{
		return SetNumPri(Row - 1, Col - 1, Num - 1);
	}

	void SetLine(int Row, ...)
	{
		va_list Num;
		va_start(Num, Row);
		for (int I = 0; I <= 8; ++I)
		{
			int v = va_arg(Num, int);
			if (v > 0 && !SetNumPri(Row - 1, I, v - 1))
			{
				throw "SetLine Error At: " + to_string(Row) + ", " + to_string(I + 1);
			}
		}
		va_end(Num);
	}

	int* Calculate()
	{
		int I, K;
		stack<vector<int>> Q;

		AppendString("Init Matrix");

		K = FindMinCell();

		while (K != -1)
		{
			if (K == -2)
			{
				if (Q.size() == 0)
				{
					AppendString("Error!!!!", false);
					return NULL;
				}

				vector<int> L = Q.top();
				Q.pop();

				K = L.back();
				L.pop_back();

				I = L.back() + 1;
				L.pop_back();

				AppendString("Stack Pop " + to_string(Q.size() + 1), false);

				RestoreNum(L);

				K = FindNextK(Q, L, K, I);
			}
			else
			{
				vector<int> L;
				for (I = 0; I <= 80; ++I)
				{
					L.push_back(_Num[I]);
				}

				K = FindNextK(Q, L, K, 1);
			}
		}

		AppendString("Calculating Complete!!!!");

		int* V = new int[81];
		for (I = 0; I <= 80; ++I)
		{
			V[I] = -_Num[I];
		}

		return V;
	}

	string CalculationString()
	{
		return _S;
	}
};

int main()
{
	Sudoku tS;
	
	try {
		tS.SetLine(1, 4, 0, 0, 0, 6, 0, 0, 0, 0);
		tS.SetLine(2, 0, 0, 0, 0, 4, 0, 5, 0, 0);
		tS.SetLine(3, 0, 0, 0, 0, 0, 0, 1, 0, 0);
		tS.SetLine(4, 2, 0, 0, 0, 0, 0, 0, 6, 7);
		tS.SetLine(5, 0, 0, 0, 1, 0, 3, 0, 0, 0);
		tS.SetLine(6, 0, 0, 0, 7, 0, 0, 0, 0, 0);
		tS.SetLine(7, 0, 3, 1, 5, 0, 0, 0, 0, 0);
		tS.SetLine(8, 0, 0, 0, 0, 0, 0, 2, 8, 0);
		tS.SetLine(9, 0, 5, 0, 0, 0, 0, 0, 0, 0);
	}
	catch(string SetLineError) {
		cout << SetLineError << endl;
		return -1;
	}

	clock_t start, end;
	start = clock();

	int* result = tS.Calculate();

	end = clock();
	cout << end - start << endl;

	if (result != NULL)
	{
		for (int I = 0; I <= 80; ++I)
		{
			cout << result[I];
			if (I % 9 == 8)
			{
				cout << endl;
			}
			else
			{
				cout << ends;
			}
		}
	}

	ofstream f("1.txt");
	f << tS.CalculationString();
	f.close();

	delete[] result;
	getchar();
	return 0;
}

