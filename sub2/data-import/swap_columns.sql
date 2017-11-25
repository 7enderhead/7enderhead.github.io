UPDATE route SET number=(@temp:=number), number = type, type = @temp;
