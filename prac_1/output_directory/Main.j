.class public Main
.super java/lang/Object
.method public <init>()V
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method

.method public static main([Ljava/lang/String;)V
.limit stack 4
.limit locals 4
invokestatic Main.main()Ljava/lang/Object;
astore_1
getstatic java/lang/System.out Ljava/io/PrintStream;
aload_1
invokevirtual java/io/PrintStream/println(Ljava/lang/Object;)V
return
.end method

.method public static main()Ljava/lang/Object;
.limit stack 500
.limit locals 500

ldc 3
invokestatic java/lang/Integer.valueOf(I)Ljava/lang/Integer;
areturn
Label0:
.end method

