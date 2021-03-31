// **************
// DISPLAY CONFIG
// **************

const RULES = {
    'axiom': '<span class="italic">ax</span>',
    'tensor': '⊗',
    'par': '<span class="flip">&</span>',
    'with': '&',
    'plus_left': '⊕<sub>1</sub>',
    'plus_right': '⊕<sub>2</sub>',
    'one': '1',
    'bottom': '⊥',
    'top': '⊤',
    // rule zero does not exist
    'promotion': '!',
    'dereliction': '?<span class="italic">d</span>',
    'contraction': '?<span class="italic">c</span>',
    'weakening': '?<span class="italic">w</span>'
};

// *************
// PROOF DISPLAY
// *************

function initProof(sequentAsJson, $container) {
    let $div = $('<div>', {'class': 'proofIsIncomplete'});
    let $div2 = $('<div>', {'class': 'proof'});
    let $sequentTable = createSequentTable(sequentAsJson);
    $div2.append($sequentTable);
    $div.append($div2);
    $container.append($div);
}

function createSequentTable(sequentAsJson) {
    let $table = $('<table>');

    let $td = $('<td>');
    $td.append(createSequent(sequentAsJson));
    $table.append($td);

    let $tagBox = $('<td>', {'class': 'tagBox'})
        .html('&nbsp;');
    $table.append($tagBox);

    return $table;
}

// ************
// PROOF UPDATE
// ************

function applyRule(rule, $sequentDiv, formulaPositions) {
    let $container = $sequentDiv.closest('.proof-container');

    // Sequent json that was stored in div may have been permuted before rule applying
    let sequentWithoutPermutation = $sequentDiv.data('sequentWithoutPermutation');
    let permutationBeforeRule = getSequentPermutation($sequentDiv);
    let sequent = permuteSequent(sequentWithoutPermutation, permutationBeforeRule);

    $.ajax({
        type: 'POST',
        url: '/apply_rule',
        contentType:'application/json; charset=utf-8',
        data: JSON.stringify({ rule, sequent, formulaPositions }),
        success: function(data)
        {
            console.log(data);
            if (data.success === true) {
                cleanPedagogicError($container);
                addSequentListPremisses($sequentDiv, permutationBeforeRule, rule, formulaPositions, data['sequentList']);
            } else {
                displayPedagogicError(data['errorMessage'], $container);
            }
        },
        error: function(jqXHR, textStatus, errorThrown) {
            console.log(jqXHR);
            console.log(jqXHR.responseText);
            console.log(textStatus);
            console.log(errorThrown);
            alert('Technical error, check browser console for more details.');
        }
    });
}

function getSequentPermutation($sequentDiv) {
    return {
        'hyp': getFormulasPermutation($sequentDiv.find('ul.hyp')),
        'cons': getFormulasPermutation($sequentDiv.find('ul.cons'))
    };
}

function getFormulasPermutation($ul) {
    let permutation = [];

    $ul.find('li').each(function(i, obj) {
        let initialPosition = $(obj).data('initialPosition');
        permutation.push(initialPosition);
    })

    return permutation;
}

function permuteSequent(sequentWithoutPermutation, sequentPermutation) {
    return {
        'hyp': permuteFormulas(sequentWithoutPermutation['hyp'], sequentPermutation['hyp']),
        'cons': permuteFormulas(sequentWithoutPermutation['cons'], sequentPermutation['cons'])
    };
}

function permuteFormulas(formulasWithoutPermutation, formulasPermutation) {
    let newFormulas = [];

    for (let initialPosition of formulasPermutation) {
        newFormulas.push(formulasWithoutPermutation[initialPosition]);
    }

    return newFormulas;
}

function addSequentListPremisses($sequentDiv, permutationBeforeRule, rule, formulaPositions, sequentList) {
    // Save data
    $sequentDiv
        .data('permutationBeforeRule', permutationBeforeRule)
        .data('rule', rule)
        .data('formulaPositions', formulaPositions);

    // Add line
    let $td = $sequentDiv.closest('td');
    $td.addClass('inference');

    // Add rule symbol
    $td.next('.tagBox')
        .html($('<div>', {'class': `tag ${rule}`})
            .html(RULES[rule]));

    // Remove old premises if any
    let $table = $td.closest('table');
    $table.prevAll().each(function (i, e) {
        e.remove();
    });
    $table.removeClass('binary-rule');

    // Mark proof as incomplete
    let $container = $table.closest('.proof-container');
    markAsIncomplete($container);

    // Add new sequents
    if (sequentList.length === 0) {
        markAsCompleteIfProofIsComplete($container);
    } else if (sequentList.length === 1) {
        createSequentTable(sequentList[0]).insertBefore($table);
    } else {
        let $div = $('<div>');
        for (let sequent of sequentList) {
            let $sibling = $('<div>', {'class': 'sibling'})
            $sibling.append(createSequentTable(sequent))
            $div.append($sibling);
        }
        $table.addClass('binary-rule');
        $div.insertBefore($table);
    }
}

// ***************
// PEDAGOGIC ERROR
// ***************

function displayPedagogicError(errorMessage, $container) {
    let $div = $container
        .children('div.pedagogic-error');
    if (!$div.length) {
        $div = $('<div>', {'class': 'pedagogic-error'});
        $div.append($('<div>', {'class': 'message'}));
        let $close = $('<div>', {'class': 'close-button'});
        $close.html('✖');
        $close.on('click', function () {cleanPedagogicError($container);});
        $div.append($close);
        $container.append($div);
    }
    $div.children('div.message').text(errorMessage);
}

function cleanPedagogicError($container) {
    $container
        .children('div.pedagogic-error')
        .remove();
}

// **********************
// CHECK PROOF COMPLETION
// **********************

function markAsComplete($container) {
    let $mainDiv = $container.children('div');
    $mainDiv.removeClass('proofIsIncomplete');
    $mainDiv.addClass('proofIsDone');
}

function markAsIncomplete($container) {
    let $mainDiv = $container.children('div');
    $mainDiv.removeClass('proofIsDone');
    $mainDiv.addClass('proofIsIncomplete');
}

function markAsCompleteIfProofIsComplete($container) {
    let $mainTable = $container
        .children('div')
        .children('div.proof')
        .children('table')
        .last();

    // We get proof stored in HTML
    let proofAsJson = recGetProofAsJson($mainTable);

    // We check if proof is complete
    checkProofIsComplete(proofAsJson, function() {markAsComplete($container);});
}

function recGetProofAsJson($table) {
    let $sequentDiv = $table.find('div.sequent');
    let sequentWithoutPermutation = $sequentDiv.data('sequentWithoutPermutation');
    let rule = $sequentDiv.data('rule') || null;
    let appliedRule = null;
    if (rule !== null) {
        let formulaPositions = $sequentDiv.data('formulaPositions');
        let $prev = $table.prev();
        let premises = [];
        if ($prev.length) {
            if ($prev.prop('tagName') === 'TABLE') {
                premises = [recGetProofAsJson($prev)];
            } else {
                $prev.children('div.sibling').each(function (i, sibling) {
                    premises.push(recGetProofAsJson($(sibling).children('table').last()));
                })
            }
        }
        appliedRule = { rule, formulaPositions, premises };

        let permutationBeforeRule = $sequentDiv.data('permutationBeforeRule');
        if (!isIdentitySequentPermutation(permutationBeforeRule)) {
            let sequentWithPermutation = permuteSequent(sequentWithoutPermutation, permutationBeforeRule);
            appliedRule = {
                rule: 'exchange',
                formulaPositions: permutationBeforeRule['cons'],
                premises: [{sequentAsJson: sequentWithPermutation, appliedRule}]
            }
        }
    }

    return { sequentAsJson: sequentWithoutPermutation, appliedRule };
}

function isIdentitySequentPermutation(sequentPermutation) {
    return isIdentity(sequentPermutation['hyp']) && isIdentity(sequentPermutation['cons']);
}

function isIdentity(permutation) {
    for (let i = 0; i < permutation.length; i++) {
        if (i !== permutation[i]) {
            return false;
        }
    }

    return true;
}

function checkProofIsComplete(proofAsJson, callbackIfComplete) {
    checkProofIsCompleteByAPI(proofAsJson, callbackIfComplete);

    // if (recCheckIsComplete(proofAsJson)) {
    //     callbackIfComplete();
    // }
}

function recCheckIsComplete(proofAsJson) {
    if (proofAsJson.appliedRule === null) {
        return false;
    }

    let response = true;

    for (let premiss of proofAsJson.appliedRule.premises) {
        response = response && recCheckIsComplete(premiss);
    }

    return response;
}

function checkProofIsCompleteByAPI(proofAsJson, callbackIfComplete) {
    console.log(proofAsJson);
    $.ajax({
        type: 'POST',
        url: '/is_proof_complete',
        contentType:'application/json; charset=utf-8',
        data: JSON.stringify(proofAsJson),
        success: function(data)
        {
            console.log(data);
            if (data['is_complete'] === true) {
                callbackIfComplete();
            }
        },
        error: function(jqXHR, textStatus, errorThrown) {
            console.log(jqXHR);
            console.log(jqXHR.responseText);
            console.log(textStatus);
            console.log(errorThrown);
            alert('Technical error, check browser console for more details.');
        }
    });
}
