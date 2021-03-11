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
}

module View = {
  let createDeleteNotif = (post: Post.t, postDiv, id, intervalId) => {
    let deleteDiv =
      Tag.createTag("div")
      ->Tag.addId(`block-${Belt.Int.toString(id)}`)
      ->Tag.addClass("post-deleted pt-1")

    let postHeadingTag =
      Tag.createTag("p")->Tag.addClass("text-center")->Tag.createTextnode(Tag.deleteText(post))
    Tag.appendChild(postHeadingTag, deleteDiv)

    let buttonDiv = Tag.createTag("div")->Tag.addClass("flex-center")
    let restoreButton =
      Tag.createTag("button")
      ->Tag.addId(`block-restore-${Belt.Int.toString(id)}`)
      ->Tag.addClass("button button-warning mr-1")
      ->Tag.createTextnode("Restore")
    let _ = restoreButton["addEventListener"]("click", () => {
      Function.restore(postDiv, id)
      Function.clearInterval(intervalId)
    })
    Tag.appendChild(restoreButton, buttonDiv)

    let deleteImmediatelyButton =
      Tag.createTag("button")
      ->Tag.addId(`block-delete-immediate-${Belt.Int.toString(id)}`)
      ->Tag.addClass("button button-danger")
      ->Tag.createTextnode("Delete Immediately")

    let _ = deleteImmediatelyButton["addEventListener"]("click", () => {
      Function.deleteElement(id)
      Function.clearInterval(intervalId)
    })
    Tag.appendChild(deleteImmediatelyButton, buttonDiv)
    Tag.appendChild(buttonDiv, deleteDiv)

    let animationDiv = Tag.createTag("div")->Tag.addClass("post-deleted-progress")
    Tag.appendChild(animationDiv, deleteDiv)
    deleteDiv
  }

  let showDeleteNotif = (post, index) => {
    let postDiv = Tag.findElementbyId(`block-${Belt.Int.toString(index)}`)
    let intervalId = window["setTimeout"](() => Function.deleteElement(index), 3000)
    let deleteDiv = createDeleteNotif(post, postDiv, index, intervalId)
    let _ = body["insertBefore"](deleteDiv, postDiv)
    body["removeChild"](postDiv)
  }

  let createPostView = (post: Post.t, id) => {
    let postDiv =
      Tag.createTag("div")->Tag.addId(`block-${Belt.Int.toString(id)}`)->Tag.addClass("post")

    let postHeadingTag =
      Tag.createTag("h2")->Tag.addClass("post-heading")->Tag.createTextnode(post.title)
    Tag.appendChild(postHeadingTag, postDiv)

    let postAuthorTag = Tag.createTag("h3")->Tag.createTextnode(post.author)
    Tag.appendChild(postAuthorTag, postDiv)

    post.text->Belt_Array.forEach(line => {
      Tag.createTag("p")
      ->Tag.addClass("post-text")
      ->Tag.createTextnode(line)
      ->Tag.appendChild(postDiv)
    })

    let button =
      Tag.createTag("button")
      ->Tag.addId(`block-${Belt.Int.toString(id)}`)
      ->Tag.addClass("button button-danger")
      ->Tag.createTextnode("Remove this post")
    let _ = button["addEventListener"]("click", () => showDeleteNotif(post, id))
    Tag.appendChild(button, postDiv)
    postDiv
  }
}

module DelayedPostRemoval = {
  let createPost = () =>
    posts->Belt_Array.forEachWithIndex((index, post) => {
      let postDiv = View.createPostView(post, index)
      Tag.appendChild(postDiv, body)
    })
}

DelayedPostRemoval.createPost()
