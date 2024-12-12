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

import java.util.ArrayList;
import java.util.List;
import org.antlr.v4.runtime.Vocabulary;
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.misc.IntervalSet;

public class IntervalSetSimplifiedWrapper extends IntervalSet {
    
    public IntervalSetSimplifiedWrapper(IntervalSet set) {
        super(set);
    }
    
    public List<String> toStringList(Vocabulary vocabulary) {
        List<String> list = new ArrayList<>();
        if (this.intervals != null && !this.intervals.isEmpty()) {
            
            for (Interval I : this.intervals) {
                int a = I.a;
                int b = I.b;
                if (a == b) {
                    list.add(this.elementName(vocabulary, a));
                } else {
                    for (int i = a; i <= b; ++i) {
                        list.add(this.elementName(vocabulary, i));
                    }
                }
            }
        }
        return list;
    }
}
