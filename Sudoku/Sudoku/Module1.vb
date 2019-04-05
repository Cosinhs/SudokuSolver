Module Module1
    Public Class Sudoku
        Private _Num(80) As Integer
        Private _V(9) As Integer
        Private _S As System.Text.StringBuilder
        Private _HasString As Boolean

        Public Sub New(Optional ByVal HasString As Boolean = True)
            Dim I As Integer
            _V(0) = 1
            For I = 1 To 8
                _V(I) = _V(I - 1) * 2
            Next
            _V(9) = 511

            For I = 0 To 80
                _Num(I) = _V(9)
            Next

            _S = New System.Text.StringBuilder
            _HasString = HasString
        End Sub

        Private Function Get1Count(ByVal Value As Integer) As Integer
            Dim C As Integer = 0
            Do While Value > 0
                Value = Value And (Value - 1)
                C += 1
            Loop
            Return C
        End Function

        Private Function RemoveNum(ByVal Row As Integer, ByVal Col As Integer, ByVal Num2 As Integer) As Integer
            Dim Index As Integer = Row * 9 + Col
            If _Num(Index) > 0 Then _Num(Index) = _Num(Index) And Num2
            Return _Num(Index)
        End Function

        Public Function SetNum(ByVal Row As Integer, ByVal Col As Integer, ByVal Num As Integer) As Boolean
            Return SetNumPri(Row - 1, Col - 1, Num - 1)
        End Function

        Public Function SetLine(ByVal Row As Integer, ByVal ParamArray Num() As Integer) As Boolean
            If Num.Length = 0 Then Return True

            Dim I As Integer

            For I = 0 To IIf(Num.Length - 1 > 8, 8, Num.Length - 1)
                If Num(I) > 0 AndAlso SetNumPri(Row - 1, I, Num(I) - 1) = False Then Return False
            Next

            Return True

        End Function

        Private Function SetNumPri(ByVal Row As Integer, ByVal Col As Integer, ByVal Num As Integer) As Boolean
            If (_V(Num) And _Num(Row * 9 + Col)) = 0 Then Return False
            _Num(Row * 9 + Col) = -(Num + 1)
            Num = _V(9) - _V(Num)

            Dim I, J As Integer

            For I = 0 To 8
                If RemoveNum(I, Col, Num) = 0 OrElse RemoveNum(Row, I, Num) = 0 Then Return False
            Next

            Dim R1 As Integer = Int(Row / 3) * 3
            Dim C1 As Integer = Int(Col / 3) * 3

            For I = R1 To R1 + 2
                For J = C1 To C1 + 2
                    If RemoveNum(I, J, Num) = 0 Then Return False
                Next
            Next

            Return True
        End Function

        Private Function SetNumPri(ByVal Index As Integer, ByVal Num2 As Integer) As Boolean
            Dim Row As Integer = Int(Index / 9)
            Dim Col As Integer = Index Mod 9
            Dim I As Integer
            For I = 0 To 8
                If _V(I) = Num2 Then Exit For
            Next
            Return SetNumPri(Row, Col, I)
        End Function

        Private Function FindMinCell() As Integer
            Dim I, C As Integer
            Dim tP As Integer = -1, tMin As Integer = 20

            I = 0

            Do
                If _Num(I) > 0 Then
                    C = Get1Count(_Num(I))
                    If C = 1 Then
                        If SetNumPri(I, _Num(I)) = False Then Return -2

                        AppendString("SetNum " & IndexToXY(I))

                        If I = tP Then
                            tP = -1
                            tMin = 20
                        End If

                        I = -1
                    Else
                        If C < tMin Then
                            tP = I
                            tMin = C
                        End If
                    End If
                End If
                I += 1
            Loop Until I > 80

            Return tP
        End Function

        Public Function Calculate() As Integer()
            Dim I As Integer
            Dim K As Integer
            Dim Q As New Stack(Of List(Of Integer))
            Dim L As List(Of Integer)

            AppendString("Init Matrix")

            K = FindMinCell()

            Do While K <> -1
                If K = -2 Then
                    If Q.Count = 0 Then
                        AppendString("Error!!!!!", False)
                        Return Nothing
                    End If


                    L = Q.Pop

                    K = L(82)
                    L.RemoveAt(82)

                    I = L(81) + 1
                    L.RemoveAt(81)

                    AppendString("Stack Pop " & Q.Count + 1, False)

                    RestoreNum(L)

                    K = FindNextK(Q, L, K, I)

                Else
                    L = New List(Of Integer)
                    L.AddRange(_Num)

                    K = FindNextK(Q, L, K, 1)

                End If

            Loop

            AppendString("Calculating Complete!!!!")

            Dim V(80) As Integer
            For I = 0 To 80
                V(I) = -_Num(I)
            Next
            Return V
        End Function

        Private Sub RestoreNum(ByVal L As List(Of Integer))
            Dim I As Integer
            For I = 0 To 80
                _Num(I) = L.Item(I)
            Next

            AppendString("Restore Matrix")
        End Sub

        Private Function GetIndexOfNum(ByVal Num As Integer, ByVal Index As Integer) As Integer
            Dim I As Integer, K As Integer = 0
            For I = 0 To 8
                If (_V(I) And Num) <> 0 Then
                    K += 1
                    If K = Index Then Return I + 1
                End If
            Next
            Return -1
        End Function

        Private Function FindNextK(ByVal Q As Stack(Of List(Of Integer)), ByVal L As List(Of Integer), ByVal K As Integer, ByVal Index As Integer) As Integer

            Dim J As Integer = GetIndexOfNum(_Num(K), Index)

            Do While J <> -1
                If SetNumPri(K, _V(J - 1)) = True Then
                    AppendString("Stack Push " & Q.Count + 1, False)
                    AppendString("SetNum MayBe " & IndexToXY(K))

                    L.Add(Index)
                    L.Add(K)
                    Q.Push(L)

                    K = FindMinCell()

                    Exit Do
                End If

                'RestoreNum(L)
                Index += 1
                J = GetIndexOfNum(_Num(K), Index)
            Loop
            If J = -1 Then K = -2
            Return K
        End Function

        Private Function ReturnNumString(ByVal Num As Integer) As String
            If Num < 0 Then Return ("#" & (-Num)).PadRight(10)
            Dim I As Integer, S As String = ""
            For I = 0 To 8
                If (_V(I) And Num) <> 0 Then S &= (I + 1)
            Next
            Return S.PadRight(10)
        End Function

        Private Function ReturnMatrix() As String
            Dim I, J As Integer, S As String = ""
            For I = 0 To 8
                For J = 0 To 8
                    S &= ReturnNumString(_Num(I * 9 + J))
                Next
                S &= vbNewLine
            Next
            Return S
        End Function

        Private Sub AppendString(ByVal Text As String, Optional ByVal AppendMatrix As Boolean = True)
            If _HasString = False Then Exit Sub
            _S.AppendLine(Text)
            _S.AppendLine()
            If AppendMatrix = True Then
                _S.AppendLine(ReturnMatrix)
                _S.AppendLine()
            End If
        End Sub

        Private Function IndexToXY(ByVal Index As Integer) As String
            Return (Int(Index / 9) + 1) & "-" & (Index Mod 9 + 1) & " Num:" & -_Num(Index)
        End Function

        Public Function CalculationString() As String
            Return _S.ToString
        End Function
    End Class

    Sub Main()
        Dim tS As New Sudoku

        Debug.Assert(tS.SetLine(1, 8, 0, 0, 0, 0, 0, 0, 0, 0))
        Debug.Assert(tS.SetLine(2, 0, 0, 3, 6, 0, 0, 0, 0, 0))
        Debug.Assert(tS.SetLine(3, 0, 7, 0, 0, 9, 0, 2, 0, 0))
        Debug.Assert(tS.SetLine(4, 0, 5, 0, 0, 0, 7, 0, 0, 0))
        Debug.Assert(tS.SetLine(5, 0, 0, 0, 0, 4, 5, 7, 0, 0))
        Debug.Assert(tS.SetLine(6, 0, 0, 0, 1, 0, 0, 0, 3, 0))
        Debug.Assert(tS.SetLine(7, 0, 0, 1, 0, 0, 0, 0, 6, 8))
        Debug.Assert(tS.SetLine(8, 0, 0, 8, 5, 0, 0, 0, 1, 0))
        Debug.Assert(tS.SetLine(9, 0, 9, 0, 0, 0, 0, 4, 0, 0))

        tS.Calculate()

        My.Computer.FileSystem.WriteAllText("1.txt", tS.CalculationString, False)
    End Sub

End Module