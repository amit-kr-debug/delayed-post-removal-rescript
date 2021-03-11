@val external document: {..} = "document"
@val external window: {..} = "window"
let body = document["body"]

module Post = {
  type t = {
    title: string,
    author: string,
    text: array<string>,
  }

  let make = (~title, ~author, ~text) => {title: title, author: author, text: text}
  let title = t => t.title
  let author = t => t.author
  let text = t => t.text
}

let posts = [
  Post.make(
    ~title="The Razor's Edge",
    ~author="W. Somerset Maugham",
    ~text=[
      "\"I couldn't go back now. I'm on the threshold. I see vast lands of the spirit stretching out before me,
    beckoning, and I'm eager to travel them.\"",
      "\"What do you expect to find in them?\"",
      "\"The answers to my questions. I want to make up my mind whether God is or God is not. I want to find out why
    evil exists. I want to know whether I have an immortal soul or whether when I die it's the end.\"",
    ],
  ),
  Post.make(
    ~title="Ship of Destiny",
    ~author="Robin Hobb",
    ~text=[
      "He suddenly recalled a callow boy telling his tutor that he dreaded the sea voyage home, because he would have
        to be among common men rather than thoughtful acolytes like himself. What had he said to Berandol?",
      "\"Good enough men, but not like us.\"",
      "Then, he had despised the sort of life where simply getting from day to day prevented a man from ever taking
        stock of himself. Berandol had hinted to him then that a time out in the world might change his image of folk
        who labored every day for their bread. Had it? Or had it changed his image of acolytes who spent so much time in
        self-examination that they never truly experienced life?",
    ],
  ),
  Post.make(
    ~title="A Guide for the Perplexed: Conversations with Paul Cronin",
    ~author="Werner Herzog",
    ~text=[
      "Our culture today, especially television, infantilises us. The indignity of it kills our imagination. May I propose a Herzog dictum? Those who read own the world. Those who watch television lose it. Sitting at home on your own, in front of the screen, is a very different experience from being in the communal spaces of the world, those centres of collective dreaming. Television creates loneliness. This is why sitcoms have added laughter tracks which try to cheat you out of your solitude. Television is a reflection of the world in which we live, designed to appeal to the lowest common denominator. It kills spontaneous imagination and destroys our ability to entertain ourselves, painfully erasing our patience and sensitivity to significant detail.",
    ],
  ),
]

module Tag = {
  let createTag = tag => {
    document["createElement"](tag)
  }

  let addClass = (element, cl) => {
    element["className"] = cl
    element
  }

  let addId = (element, id) => {
    element["setAttribute"]("id", id)
    element
  }

  let appendChild = (child, parent): unit => {
    parent["appendChild"](child)
  }

  let createTextnode = (element, text) => {
    element["innerHTML"] = text
    element
  }

  let findElementbyId = id => {
    document["getElementById"](id)
  }

  let deleteText = (post: Post.t) => {
    `This post from <em>${post.title} by ${post.author}</em> will be permanently removed in 10 seconds.`
  }
}

module Function = {
  let restore = (postDiv, post_id): unit => {
    let deleteDiv = Tag.findElementbyId(`block-${Belt.Int.toString(post_id)}`)
    let _ = body["insertBefore"](postDiv, deleteDiv)
    body["removeChild"](deleteDiv)
  }

  let deleteElement = (post_id): unit => {
    let deleteDiv = Tag.findElementbyId(`block-${Belt.Int.toString(post_id)}`)
    body["removeChild"](deleteDiv)
  }
  let clearInterval = (intervalId): unit => {
    window["clearInterval"](intervalId)
  }

  let configureDeleteNotifButtons = (index, postDiv, intervalId) => {
    let restoreButton = Tag.findElementbyId(`block-restore-${Belt.Int.toString(index)}`)
    let _ = restoreButton["addEventListener"]("click", () => {
      restore(postDiv, index)
      clearInterval(intervalId)
    })
    let deleteImmediatelyButton = Tag.findElementbyId(
      `block-delete-immediate-${Belt.Int.toString(index)}`,
    )
    let _ = deleteImmediatelyButton["addEventListener"]("click", () => {
      deleteElement(index)
      clearInterval(intervalId)
    })
  }
}

module View = {
  let createDeleteNotif = (post: Post.t, id) => {
    let deleteDivInnerHtml = `<p class="text-center">
        This post from <em>${post.title} by ${post.author}</em> will be permanently removed in 10 seconds.
      </p>
      <div class="flex-center">
        <button id="block-restore-${Belt.Int.toString(
        id,
      )}" class="button button-warning mr-1">Restore</button>
        <button id="block-delete-immediate-${Belt.Int.toString(
        id,
      )}" class="button button-danger">Delete Immediately</button>
      </div>
      <div class="post-deleted-progress"></div>`

    let deleteDiv =
      Tag.createTag("div")
      ->Tag.addId(`block-${Belt.Int.toString(id)}`)
      ->Tag.addClass("post-deleted pt-1")
      ->Tag.createTextnode(deleteDivInnerHtml)

    deleteDiv
  }

  let showDeleteNotif = (post, index) => {
    let postDiv = Tag.findElementbyId(`block-${Belt.Int.toString(index)}`)
    let intervalId = window["setTimeout"](() => Function.deleteElement(index), 10000)
    let deleteDiv = createDeleteNotif(post, index)
    let _ = body["insertBefore"](deleteDiv, postDiv)
    let _ = body["removeChild"](postDiv)
    Function.configureDeleteNotifButtons(index, postDiv, intervalId)
  }

  let createPostView = (post: Post.t, id) => {
    let postDivInnerHtml =
      `<h2 class="post-heading">${post.title}</h2>
                            <h3>${post.author}</h3>` ++
      post.text->Belt.Array.reduce("", (text, line) => {
        text ++ `<p class="post-text">${line}</p>`
      }) ++
      `<button id= "block-delete-${Belt.Int.toString(
          id,
        )}" class="button button-danger">Remove</button>`

    let postDiv =
      Tag.createTag("div")
      ->Tag.addId(`block-${Belt.Int.toString(id)}`)
      ->Tag.addClass("post")
      ->Tag.createTextnode(postDivInnerHtml)

    postDiv
  }
}

module DelayedPostRemoval = {
  let createPost = () =>
    posts->Belt_Array.forEachWithIndex((index, post) => {
      let postDiv = View.createPostView(post, index)
      Tag.appendChild(postDiv, body)
      let removeButton = Tag.findElementbyId(`block-delete-${Belt.Int.toString(index)}`)
      let _ = removeButton["addEventListener"]("click", () => View.showDeleteNotif(post, index))
    })
}

DelayedPostRemoval.createPost()
