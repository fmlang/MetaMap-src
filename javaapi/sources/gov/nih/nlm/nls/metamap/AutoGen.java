package gov.nih.nlm.nls.metamap;

import java.io.*;
import se.sics.prologbeans.*;

public class AutoGen {
  private PrologSession session = new PrologSession();

  public AutoGen() {
    session.setTimeout(30000);
  }

  /** process a string containing one or more documents - unicode (utf8) is not supported
   * @param aString a file of documents
   * @return a result instance
   */
  public Term processString(String aString) {
    Term result = null;
    try {
      System.out.println("aString: " + aString);
      Bindings bindings = new Bindings().bind("E", aString);
      System.out.println("bindings: " + bindings.toString());
      QueryAnswer answer =
	session.executeQuery("process_string(E,Output)", bindings);
      result = answer.getValue("Output");
      System.out.println("answer: " + result.toString());
      if (result == null) {
        System.err.println("Error: " + answer.getError() + "\n");
      }
    } catch (Exception e) {
      System.err.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
      e.printStackTrace();
    }
    return result;
  }

  public void inspect(Term aTerm, int level) {
    if (aTerm.isList()) {
      this.traverselist((PBList)aTerm, level+1);
    } else if (aTerm.isString()) {
      System.out.print( aTerm.getName());
    } else if (aTerm.isAtom()) {
      System.out.print( aTerm.getName());
    } else if (aTerm.isInteger()) {
      System.out.print( aTerm.getName());
    } else if (aTerm.isCompound()) {
      System.out.print( aTerm.getName() + "(");
      for (int i = 1; i < aTerm.getArity(); i++) {
	Term subTerm = aTerm.getArgument(i);
	this.inspect(subTerm, level+1);
	if (i < aTerm.getArity()) System.out.print( "," ) ;
      }
      System.out.println(")");
    }
  }

  public void traverselist(PBList pbList, int level)
  {
    System.out.print( "[");
    for (int i = 1; i< pbList.getLength(); i++) {
      Term aTerm = pbList.getTermAt(i);
      this.inspect(aTerm, level+1);
      if (i < pbList.getLength()) System.out.print( "," ) ;
    }
    System.out.print("]");
  }

  public static void main(String[] args) 
    throws Exception
  {
    AutoGen frontEnd = new AutoGen();
    
    if (args.length < 1) {
      System.out.println("usage: gov.nih.nlm.nls.metamap.AutoGen term");
      System.exit(0);
    }
    StringBuffer sb = new StringBuffer();
    for (String arg: args) {
      sb.append(arg).append(" ");
    }
    Term mmoTerm = frontEnd.processString(sb.toString());
    System.out.println("mmoTerm: " + mmoTerm);
    frontEnd.inspect(mmoTerm, 0);
  }
}