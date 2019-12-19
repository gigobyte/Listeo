import React, { useState } from 'react'
import styled from 'styled-components'
import { colors } from './color'
import { Input, useInput } from './Input'
import { Icons } from './Icon'
import { rule, ifRegexFails, fail, ifBlank } from './validate'

interface TagInputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  'data-test'?: string
  tags: string[]
  onAddTag: (tag: string) => void
  onRemoveTag: (tag: string) => void
}

enum TagInputValidationError {
  TagMissing = 'Please enter a tag first',
  InvalidTag = 'Please enter a valid tag, the only special characters allowed are - and _',
  TagLimitReached = 'You can only add up to 10 tags'
}

const Wrapper = styled.div`
  display: flex;
  flex-direction: column;
  align-items: center;
`

const TagContent = styled.div`
  display: inline-block;
  color: ${colors.white};
  background-color: ${colors.blue200};
  padding: 4px;
  border-radius: 2px 0 0 2px;
`

const TagsContainer = styled.div`
  display: flex;
  padding-bottom: 10px;
`

const TagContainer = styled.div`
  display: flex;
`

const TagRemoveButton = styled.button`
  background-color: ${colors.blue300};
  border: 0;
  cursor: pointer;
  margin-right: 7px;
  height: 26px;
  color: ${colors.white};
  border-radius: 0 2px 2px 0;
  &:focus {
    outline: none;
  }
`

export const TagInput: React.FC<TagInputProps> = ({
  tags,
  placeholder,
  onRemoveTag,
  onAddTag,
  'data-test': dataTest
}) => {
  const [showError, setShowError] = useState(false)

  const input = useInput({
    trim: true,
    validations: [
      rule(ifBlank, TagInputValidationError.TagMissing),
      rule(
        ifRegexFails(/(^[A-Za-z0-9_-]+)$/),
        TagInputValidationError.InvalidTag
      ),
      rule(
        fail(() => tags.length >= 3),
        TagInputValidationError.TagLimitReached
      )
    ],
    shouldShowError: _ => showError
  })

  return (
    <Wrapper>
      <Input
        {...input}
        onChange={e => {
          setShowError(false)
          input.onChange(e)
        }}
        data-test={dataTest + '-input'}
        placeholder={placeholder}
        onKeyDown={e => {
          if (e.key === 'Enter') {
            if (input.isValid) {
              onAddTag(input.value)
              input.setValue('')
            } else {
              setShowError(true)
            }
          }
        }}
      />
      {tags.length > 0 && (
        <TagsContainer>
          {tags.map((tag, i) => (
            <TagContainer data-test={`${dataTest}-tag-${i + 1}`}>
              <TagContent>{tag}</TagContent>
              <TagRemoveButton
                data-test={`${dataTest}-tag-${i + 1}-delete`}
                onClick={() => onRemoveTag(tag)}
              >
                <Icons.times />
              </TagRemoveButton>
            </TagContainer>
          ))}
        </TagsContainer>
      )}
    </Wrapper>
  )
}

export const useTagInput = () => {
  const [tags, setTags] = useState<string[]>([])

  return {
    tags,
    onAddTag: (tag: string) => {
      setTags([...new Set([...tags, tag])])
    },
    onRemoveTag: (tag: string) => setTags(tags.filter(x => x !== tag))
  }
}
