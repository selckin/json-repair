/*
 * Copyright 2024 HAibiiin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.github.haibiiin.json.repair.antlr;

import io.github.haibiiin.json.repair.Expecting;
import io.github.haibiiin.json.repair.antlr.autogen.JSONParser;
import java.util.BitSet;
import java.util.List;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.IntervalSet;

public class SyntaxErrorListener implements ANTLRErrorListener {
    
    private DefaultErrorStrategyWrapper strategyWrapper;
    private Expecting expecting;
    
    public SyntaxErrorListener(DefaultErrorStrategyWrapper strategyWrapper, Expecting expecting) {
        this.strategyWrapper = strategyWrapper;
        this.expecting = expecting;
    }
    
    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object o, int i, int i1, String s, RecognitionException e) {
        if (recognizer instanceof JSONParser) {
            Token token = ((JSONParser) recognizer).getCurrentToken();
            String expectingKey = strategyWrapper.getTokenErrorDisplay(token);
            
            IntervalSet expectingSet = ((JSONParser) recognizer).getExpectedTokens();
            IntervalSetSimplifiedWrapper expectingWrapper = new IntervalSetSimplifiedWrapper(expectingSet);
            List<String> expectingStrs = expectingWrapper.toStringList(new VocabularyWrapper(recognizer.getVocabulary()));
            
            this.expecting.add(i1, expectingKey, expectingStrs);
        }
    }
    
    @Override
    public void reportAmbiguity(Parser parser, DFA dfa, int i, int i1, boolean b, BitSet bitSet, ATNConfigSet atnConfigSet) {
        
    }
    
    @Override
    public void reportAttemptingFullContext(Parser parser, DFA dfa, int i, int i1, BitSet bitSet, ATNConfigSet atnConfigSet) {
        
    }
    
    @Override
    public void reportContextSensitivity(Parser parser, DFA dfa, int i, int i1, int i2, ATNConfigSet atnConfigSet) {
        
    }
}
