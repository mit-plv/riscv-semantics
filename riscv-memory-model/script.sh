javac -cp alloy4.2_2015-02-22.jar RiscvSemantics.java
jar -cf riscvSemantics.jar *.class
java -classpath riscvSemantics.jar:alloy4.2_2015-02-22.jar RiscvSemantics example.als
