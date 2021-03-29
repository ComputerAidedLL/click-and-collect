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

function applyRule(rule, $sequentDiv, formulaPosition) {
    let sequent = getSequentWithPermutations($sequentDiv);
    let $container = $sequentDiv.closest('.proof-container');

    $.ajax({
        type: 'POST',
        url: '/apply_rule',
        contentType:'application/json; charset=utf-8',
        data: JSON.stringify({ rule, sequent, formulaPosition }),
        success: function(data)
        {
            console.log(data);
            if (data.success === true) {
                cleanPedagogicError($container);
                addSequentListPremisses($sequentDiv, data['sequentList'], rule);
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

function getSequentWithPermutations($sequentDiv) {
    let sequent = $sequentDiv.data('sequent');

    return {
        'hyp': getFormulasWithPermutation($sequentDiv.find('ul.hyp'), sequent['hyp']),
        'cons': getFormulasWithPermutation($sequentDiv.find('ul.cons'), sequent['cons'])
    };
}

function getFormulasWithPermutation($ul, initialFormulas) {
    let newFormulas = [];

    $ul.find('li').each(function(i, obj) {
        let initialPosition = $(obj).data('initialPosition');
        newFormulas.push(initialFormulas[initialPosition]);
    })

    return newFormulas;
}

function addSequentListPremisses($sequentDiv, sequentList, rule) {
    // Save rule
    $sequentDiv.data('rule', rule);

    // Add line
    let $td = $sequentDiv.closest('td');
    $td.addClass('inference');

    // Add rule symbol
    $td.next('.tagBox')
        .html($('<div>', {'class': `tag ${rule}`})
            .html(RULES[rule]));

    // Remove old premisses if any
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
        checkProofIsComplete($container);
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

function checkProofIsComplete($container) {
    let $mainDiv = $container
        .children('div');
    let $mainTable = $mainDiv.children('div.proof')
        .children('table').last();
    if (recCheckIsComplete(recGetProofAsJson($mainTable))) {
        $mainDiv.removeClass('proofIsIncomplete');
        $mainDiv.addClass('proofIsDone');
    }
}

function recGetProofAsJson($table) {
    let $sequentDiv = $table.find('div.sequent');
    let sequent = $sequentDiv.data('sequent');
    let rule = $sequentDiv.data('rule') || null;
    let premisses = [];
    if (rule !== null) {
        let $prev = $table.prev();
        if ($prev.length) {
            if ($prev.prop('tagName') === 'TABLE') {
                premisses = [recGetProofAsJson($prev)];
            } else {
                $prev.children('div.sibling').each(function (i, sibling) {
                    premisses.push(recGetProofAsJson($(sibling).children('table')));
                })
            }
        }
    }

    return { sequent, rule, premisses };
}

function recCheckIsComplete(proofAsJson) {
    if (proofAsJson.rule === null) {
        return false;
    }

    let response = true;

    for (let premiss of proofAsJson.premisses) {
        response = response && recCheckIsComplete(premiss);
    }

    return response;
}

function markAsIncomplete($container) {
    let $mainDiv = $container.children('div');
    $mainDiv.removeClass('proofIsDone');
    $mainDiv.addClass('proofIsIncomplete');
}