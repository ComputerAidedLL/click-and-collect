// **************
// DISPLAY CONFIG
// **************

const UNARY_CONNECTORS = {
    'negation': '<span>¬</span>',
    'ofcourse': '<span>!</span>',
    'whynot': '<span>?</span>'
};

const BINARY_CONNECTORS = {
    'implication': '<span class="binary-connector">→</span>',
    'conjunction': '<span class="binary-connector">∧</span>',
    'disjunction': '<span class="binary-connector">∨</span>',
    'tensor': '<span class="binary-connector">⊗</span>',
    'par': '<span class="binary-connector flip">&</span>',
    'with': '<span class="binary-connector">&</span>',
    'plus': '<span class="binary-connector">⊕</span>',
    'lollipop': '<span class="binary-connector">⊸</span>'
};

const NEUTRAL_ELEMENTS = {
    'true': '<span>true</span>',
    'false': '<span>false</span>',
    'one': '<span>1</span>',
    'bottom': '<span>⊥</span>',
    'top': '<span>⊤</span>',
    'zero': '<span>0</span>'
};

// ****************
// DISPLAY FUNCTION
// ****************

function createSequent(sequentAsJson) {
    let $sequentDiv = $('<div>', {'class': 'sequent'})
        .data('sequent', sequentAsJson);

    if ('hyp' in sequentAsJson) {
        createFormulaList(sequentAsJson, 'hyp', $sequentDiv);
    }

    let $thesisSpan = $('<span class="turnstile explained">⊢</span>');
    $thesisSpan.on('click', function () {
        applyRule('axiom', $sequentDiv, 0);
    });
    $sequentDiv.append($thesisSpan);

    if ('cons' in sequentAsJson) {
        createFormulaList(sequentAsJson, 'cons', $sequentDiv);
    }

    return $sequentDiv;
}

function createFormulaList(sequentAsJson, field, $sequentDiv) {
    let $ul = $('<ul>', {'class': ['commaList ' + field]})
        .sortable({
            helper : 'clone'
        });

    for (let i = 0; i < sequentAsJson[field].length; i++) {
        let formulaAsJson = sequentAsJson[field][i];
        let $li = $('<li>').data('initialPosition', i);

        // Build formula
        let $span = $('<span>', {'class': 'main-formula'})
            .html(createFormulaHTML(formulaAsJson, true));
        $li.append($span);

        // Add events (click, double-click), and classes for hover
        addEventsAndStyle($li, formulaAsJson);

        $ul.append($li);
    }

    $sequentDiv.append($ul);
}

function createFormulaHTML(formulaAsJson, isMainFormula = true) {
    switch (formulaAsJson.type) {
        case 'litteral':
            return `<span>${formulaAsJson.value}</span>`;

        case 'neutral':
            let neutralElement = NEUTRAL_ELEMENTS[formulaAsJson.value];
            if (isMainFormula) {
                return `<span class="primaryConnector">${neutralElement}</span>`;
            }
            return neutralElement;

        case 'negation':
            return UNARY_CONNECTORS[formulaAsJson.type] + createFormulaHTML(formulaAsJson.value, false);

        case 'ofcourse':
        case 'whynot':
            let unaryConnector = UNARY_CONNECTORS[formulaAsJson.type];
            let subFormula = createFormulaHTML(formulaAsJson.value, false);
            if (isMainFormula) {
                unaryConnector = `<span class="primaryConnector">${unaryConnector}</span>`;
                subFormula = `<span class="sub-formula">${subFormula}</span>`;
            }
            return unaryConnector + subFormula;

        case 'orthogonal':
            return createFormulaHTML(formulaAsJson.value, false)
                + '<span><sup>⊥</sup></span>';

        case 'implication':
        case 'conjunction':
        case 'disjunction':
        case 'tensor':
        case 'par':
        case 'with':
        case 'plus':
        case 'lollipop':
            let connector = BINARY_CONNECTORS[formulaAsJson.type];
            if (isMainFormula) {
                connector = `<span class="primaryConnector">${connector}</span>`;
            }

            let leftFormula = createFormulaHTML(formulaAsJson['value1'], false);
            let rightFormula = createFormulaHTML(formulaAsJson['value2'], false);
            if (isMainFormula) {
                leftFormula = `<span class="left-formula">${leftFormula}</span>`;
                rightFormula = `<span class="right-formula">${rightFormula}</span>`;
            }
            let formula = leftFormula + connector + rightFormula;

            if (!isMainFormula) {
                return `<span>(</span>${formula}<span>)</span>`;
            }

            return formula;

        default:
            console.error('No display rule for type ' + formulaAsJson.type);
            return '';
    }
}

// *****
// RULES
// *****

function getRules(formulaAsJson) {
    switch (formulaAsJson.type) {
        case 'litteral':
        case 'orthogonal':
            return [{'element': 'main-formula', 'onclick': ['axiom']}];

        case 'tensor':
        case 'par':
        case 'with':
            return [{'element': 'main-formula', 'onclick': [formulaAsJson.type]}];

        case 'plus':
            return [
                {'element': 'left-formula', 'onclick': ['plus_left']},
                {'element': 'right-formula', 'onclick': ['plus_right']}
            ];

        case 'neutral':
            switch (formulaAsJson.value) {
                case 'one':
                case 'top':
                case 'bottom':
                case 'zero': // click on zero will display a pedagogic error
                    return [{'element': 'main-formula', 'onclick': [formulaAsJson.value]}];

                default:
                    return [];
            }

        case 'ofcourse':
            return [{'element': 'main-formula', 'onclick': ['promotion']}];

        case 'whynot':
            return [
                {'element': 'primaryConnector', 'onclick': ['weakening', 'contraction']},
                {'element': 'sub-formula', 'onclick': ['dereliction', 'contraction']}
            ];

        default:
            return [];
    }
}

function addEventsAndStyle($li, formulaAsJson) {
    let rules = getRules(formulaAsJson);
    for (let ruleEvent of rules) {
        let $spanForEvent = $li.find('span.' + ruleEvent.element).first();

        // Some hover config
        $spanForEvent.addClass('clickableExpr');
        if (ruleEvent.element !== 'main-formula') {
            $spanForEvent.addClass('highlightableExpr');
        }

        // Add click and double click events
        if (ruleEvent.onclick.length === 1) {
            // Single click
            let rule = ruleEvent.onclick[0];
            $spanForEvent.on('click', buildApplyRuleCallBack(rule, $li));

            // Some hover config for tensor
            if (rule === 'tensor') {
                $li.addClass('tensor');
                let $rightFormula = $li.find('span' + '.right-formula').first();
                $rightFormula.addClass('tensor-right');
            }
        } else {
            // Single click AND Double click event
            let singleClickCallBack = buildApplyRuleCallBack(ruleEvent.onclick[0], $li);
            let doubleClickCallBack = buildApplyRuleCallBack(ruleEvent.onclick[1], $li);

            addClickAndDoubleClickEvent($spanForEvent, singleClickCallBack, doubleClickCallBack);
        }
    }
}

function buildApplyRuleCallBack(rule, $li) {
    return function() {
        let $sequentDiv = $li.closest('div.sequent');
        let formulaPosition = $li.parent().children().index($li);

        applyRule(rule, $sequentDiv, formulaPosition);
    }
}

// ******************
// DOUBLE CLICK EVENT
// ******************

const CLICK_DELAY = 200;
window.clickCount = 0;
window.clickTimer = null;

function addClickAndDoubleClickEvent ($element, singleClickCallBack, doubleClickCallBack) {
    // https://stackoverflow.com/a/7845282
    $element.on('click', function () {
        clickCount++;
        if (clickCount === 1) {
            window.clickTimer = setTimeout(function () {
                singleClickCallBack();
                window.clickCount = 0;
            }, CLICK_DELAY);
        } else {
            clearTimeout(window.clickTimer);
            doubleClickCallBack();
            window.clickCount = 0;
        }
    })
}